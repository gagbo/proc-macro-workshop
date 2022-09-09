use proc_macro::TokenStream;

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    seq_derive::expand(input)
}

mod seq_derive {
    use proc_macro2::{token_stream, Group, TokenStream, TokenTree};
    use quote::{quote, quote_spanned};
    use syn::{braced, parse::Parse, parse_macro_input, Error, Ident, LitInt, Result};

    const PASTE_SEP: char = '~';

    struct Seq {
        var: Ident,
        lower_bound: LitInt,
        upper_bound: LitInt,
        body: TokenStream,
    }

    impl Parse for Seq {
        fn parse(input: syn::parse::ParseStream) -> Result<Self> {
            use syn::Token;
            let var: Ident = input.parse()?;
            input.parse::<Token![in]>()?;
            let lower_bound: LitInt = input.parse()?;
            input.parse::<Token![..]>()?;
            let upper_bound: LitInt = input.parse()?;
            let loop_content;
            braced! { loop_content in input };
            // Use parse() and not cursor().token_stream() here, we HAVE to traverse this
            // ref: https://github.com/dtolnay/syn/issues/1120
            let body: TokenStream = loop_content.parse()?;

            Ok(Self {
                var,
                lower_bound,
                upper_bound,
                body,
            })
        }
    }

    impl Seq {
        /// Rewrite recursively the given token tree, replacing mentions of
        /// local with current_value.
        ///
        /// tts_cursored is a cursor into the currently traversed token tree, so
        /// that the function can skip items if we want to consume the current
        /// token `current_tt` and a few tokens that immediately follow it.
        ///
        /// The undocumented contract is that `tts_cursored` is currently
        /// pointing at an element that would be strictly equal (in value and
        /// Span) to `current_tt`, so that `tts_cursored.next()` is the
        /// TokenTree element that directly follows `current_tt`
        fn rewrite(
            current_tt: TokenTree,
            tts_cursored: &mut token_stream::IntoIter,
            local: &Ident,
            current_value: &syn::Index,
        ) -> Result<TokenStream> {
            match current_tt {
                TokenTree::Group(inner) => {
                    let span = inner.span();
                    // Extracting the delimiters to write them back in the
                    // TokenStream later as a TokenTree::Group variant
                    let delim = inner.delimiter();
                    let mut stmts = inner.stream().into_iter();
                    let mut out: Vec<TokenStream> = Vec::new();
                    while let Some(inner_tt) = stmts.next() {
                        let tt_span = inner_tt.span();
                        let gen_stream = Self::rewrite(inner_tt, &mut stmts, local, current_value)?;
                        out.push(quote_spanned! {tt_span=>
                            #gen_stream
                        });
                    }
                    let mut rewrote_inner = Group::new(
                        delim,
                        quote_spanned! {span=>
                            #(#out)*
                        },
                    );
                    // Even if we use quote_spanned to build the rewritten group,
                    // the span of the group is set by the delimiters, and the
                    // constructor Group::new sets inconditionally the span to
                    // Span::call_site() which is the entire macro basically
                    rewrote_inner.set_span(span);
                    Ok(quote! {
                    #rewrote_inner })
                }
                TokenTree::Ident(ref ident) if ident == local => {
                    let span = ident.span();
                    Ok(quote_spanned! {span=>
                    #current_value})
                }
                TokenTree::Ident(i) => {
                    // Checking whether ident is followed by
                    // PASTE_SEP and our "local" ident.
                    let mut peek = tts_cursored.clone();
                    let span = i.span();
                    let (lookahead_1, lookahead_2) = (peek.next(), peek.next());
                    match (lookahead_1, lookahead_2) {
                        (Some(TokenTree::Punct(p)), Some(l)) if p.as_char() == PASTE_SEP => {
                            // NOTE: Implementing suffix parsing would need to happen here,
                            // with a
                            // let peek_suffix = peek.clone();
                            // let (lookahead_3, lookahead_4) = (peek_suffix.next(), peek_suffix.next());
                            if let TokenTree::Ident(ref l) = l {
                                if l == local {
                                    // This is the "consumption of lookahead" step
                                    *tts_cursored = peek.clone();

                                    let new_ident =
                                        Ident::new(&format!("{}{}", i, current_value.index), span);

                                    Ok(quote_spanned! {span=>
                                    #new_ident})
                                } else {
                                    Err(Error::new_spanned(l, "Expecting the identifier {local}"))
                                }
                            } else {
                                Err(Error::new_spanned(l, "Expecting the identifier {local}"))
                            }
                        }
                        _ => Ok(quote_spanned! {span=>
                        #i}),
                    }
                }
                TokenTree::Punct(p) => {
                    let span = p.span();
                    Ok(quote_spanned! {span=>
                    #p})
                }
                TokenTree::Literal(l) => {
                    let span = l.span();
                    Ok(quote_spanned! {span=>
                    #l})
                }
            }
        }

        fn expand(self) -> Result<TokenStream> {
            let Self {
                var,
                lower_bound,
                upper_bound,
                body,
            } = self;
            let statements: Vec<TokenStream> = (lower_bound.base10_parse::<usize>()?
                ..upper_bound.base10_parse::<usize>()?)
                // Necessary step to remove the 'usize' type suffix from the produced items
                .map(syn::Index::from)
                .map(|i| {
                    let cur_val = i;
                    let local = var.clone();
                    let mut body_elems = body.clone().into_iter();
                    let mut out: TokenStream = TokenStream::new();
                    while let Some(tt) = body_elems.next() {
                        let tt_span = tt.span();
                        let gen_stream = Self::rewrite(tt, &mut body_elems, &local, &cur_val)?;
                        out.extend(quote_spanned! {tt_span=>
                            #gen_stream
                        });
                    }
                    Ok(out)
                })
                .collect::<Result<Vec<_>>>()?;
            Ok(quote! {
                #(#statements)*
            })
        }
    }

    pub fn expand(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
        let sequence = parse_macro_input!(input as Seq);
        sequence
            .expand()
            .unwrap_or_else(syn::Error::into_compile_error)
            .into()
    }
}
