use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    sorted_mac::check(args, input)
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    sorted_mac::check_function(args, input)
}

mod sorted_mac {

    use proc_macro2::{Span, TokenStream};
    use quote::{quote, ToTokens};
    use syn::{parse_macro_input, Error, Result};

    pub fn check(
        args: proc_macro::TokenStream,
        input: proc_macro::TokenStream,
    ) -> proc_macro::TokenStream {
        let input = parse_macro_input!(input as syn::Item);
        expand(args.into(), input)
            .unwrap_or_else(Error::into_compile_error)
            .into()
    }

    pub fn check_function(
        args: proc_macro::TokenStream,
        input: proc_macro::TokenStream,
    ) -> proc_macro::TokenStream {
        let input = parse_macro_input!(input as syn::Item);
        expand_function(args.into(), input)
            .unwrap_or_else(Error::into_compile_error)
            .into()
    }

    fn expand(args: TokenStream, input: syn::Item) -> Result<TokenStream> {
        match input {
            syn::Item::Enum(ref input_enum) => {
                let variants = &input_enum.variants;
                // Keeping all variants so that the error message gives
                // the exact position to move the variant to.
                let mut previous_variants: Vec<String> = Vec::new();
                for variant in variants {
                    let curr_variant = variant.ident.to_string();
                    for prev_var in previous_variants.iter() {
                        if &curr_variant < prev_var {
                            return Err(Error::new_spanned(
                                variant.ident.clone(),
                                format!("{} should sort before {}", &curr_variant, prev_var),
                            ));
                        };
                    }
                    previous_variants.push(curr_variant);
                }
                Ok(quote! { #input_enum })
            }
            _ => Err(Error::new(
                Span::call_site(),
                "expected enum or match expression",
            )),
        }
    }

    fn expand_function(args: TokenStream, mut input: syn::Item) -> Result<TokenStream> {
        match input {
            syn::Item::Fn(item_fn) => {
                // Go through the item_fn body
                let syn::ItemFn {
                    attrs,
                    vis,
                    sig,
                    block,
                } = item_fn;

                let transformed_item = syn::ItemFn {
                    attrs,
                    vis,
                    sig,
                    block: Box::new(transform_block(*block)?),
                };
                Ok(quote! { #transformed_item })
            }
            _ => Err(Error::new(Span::call_site(), "expected function")),
        }
    }

    fn transform_block(block: syn::Block) -> Result<syn::Block> {
        let syn::Block { brace_token, stmts } = block;
        let new_stmts = stmts
            .into_iter()
            .map(|stmt| match stmt {
                syn::Stmt::Expr(syn::Expr::Match(ref m)) => {
                    if m.attrs.iter().any(|attr| attr.path.is_ident("sorted")) {
                        let arms = &m.arms;
                        // Keeping all arms so that the error message gives
                        // the exact position to move the arm to.
                        let mut previous_arms: Vec<String> = Vec::new();
                        for arm in arms {
                            let curr_arm = arm.pat.to_token_stream().to_string();
                            for prev_arm in previous_arms.iter() {
                                if &curr_arm < prev_arm {
                                    return Err(Error::new_spanned(
                                        arm.pat.clone(),
                                        format!("{} should sort before {}", &curr_arm, prev_arm),
                                    ));
                                };
                            }
                            previous_arms.push(curr_arm);
                        }

                        Ok(syn::Stmt::Expr(syn::Expr::Match(syn::ExprMatch {
                            attrs: Vec::new(),
                            match_token: m.match_token,
                            expr: m.expr.clone(),
                            brace_token: m.brace_token,
                            arms: m.arms.clone(),
                        })))
                    } else {
                        Ok(stmt)
                    }
                }
                syn::Stmt::Semi(syn::Expr::Match(ref m), s) => {
                    if m.attrs.iter().any(|attr| attr.path.is_ident("sorted")) {
                        // TODO: test for sorted arms
                        // Copy paste the code from the Expr case once it runs properly
                        Ok(syn::Stmt::Semi(
                            syn::Expr::Match(syn::ExprMatch {
                                attrs: Vec::new(),
                                match_token: m.match_token,
                                expr: m.expr.clone(),
                                brace_token: m.brace_token,
                                arms: m.arms.clone(),
                            }),
                            s,
                        ))
                    } else {
                        Ok(stmt)
                    }
                }
                syn::Stmt::Expr(ref e) => Ok(stmt),
                syn::Stmt::Semi(ref e, _) => Ok(stmt),
                syn::Stmt::Local(ref e) => Ok(stmt),
                syn::Stmt::Item(ref it) => Ok(stmt),
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(syn::Block {
            brace_token,
            stmts: new_stmts,
        })
    }
}
