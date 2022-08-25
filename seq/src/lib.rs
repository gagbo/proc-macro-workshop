use proc_macro::TokenStream;

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    seq_derive::expand(input)
}

mod seq_derive {
    use proc_macro2::{Group, TokenStream, TokenTree};
    use quote::{quote, quote_spanned};
    use syn::{braced, parse::Parse, parse_macro_input, Ident, LitInt, Result};

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

            Ok(Self {
                var,
                lower_bound,
                upper_bound,
                // Use parse() and not cursor().token_stream() here, we HAVE to traverse this
                // ref: https://github.com/dtolnay/syn/issues/1120
                body: loop_content.parse()?,
            })
        }
    }

    impl Seq {
        // Rewrite recursively the given token tree, replacing mentions of local with current_value.
        fn rewrite(
            tt: TokenTree,
            local: &Ident,
            current_value: &syn::Index,
        ) -> Result<TokenStream> {
            // Ok(Default::default())
            // TODO: adding this code makes the test 2 fail actually
            match tt {
                TokenTree::Group(inner) => {
                    let span = inner.span();
                    // Extracting the delimiters to write them back in the
                    // TokenStream later as a TokenTree::Group variant
                    let delim = inner.delimiter();
                    let statements: Vec<_> = inner
                        .stream()
                        .into_iter()
                        .map(|inner_tt| -> Result<TokenStream> {
                            Self::rewrite(inner_tt, local, current_value)
                        })
                        .collect::<Result<Vec<_>>>()?;
                    let mut rewrote_inner = Group::new(
                        delim,
                        quote_spanned! {span=>
                            #(#statements)*
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
                    let span = i.span();
                    Ok(quote_spanned! {span=>
                    #i})
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
            // False positive, can't compile without this
            #[allow(clippy::map_flatten)]
            let statements: Vec<TokenStream> = (lower_bound.base10_parse::<usize>()?
                ..upper_bound.base10_parse::<usize>()?)
                // Necessary step to remove the 'usize' type suffix from the produced items
                .map(syn::Index::from)
                .map(|i| {
                    let cur_val = i;
                    let local = var.clone();
                    body.clone()
                        .into_iter()
                        .map(move |tt| -> Result<TokenStream> {
                            let tt_span = tt.span();
                            let gen_stream = Self::rewrite(tt, &local, &cur_val)?;
                            Ok(quote_spanned! {tt_span=>
                                #gen_stream
                            })
                        })
                })
                .flatten()
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
