use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    debug_derive::expand(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

mod debug_derive {

    use proc_macro2::TokenStream;
    use quote::quote;
    use syn::{
        parse_quote, punctuated::Punctuated, token::Comma, Attribute, Data, DataStruct,
        DeriveInput, Error, Field, Fields, GenericParam, Generics, Lit, Result,
    };

    pub(crate) fn expand(input: DeriveInput) -> Result<TokenStream> {
        let generics = add_trait_bounds(input.generics.clone());
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
        let st_fields = match input.data {
            Data::Struct(DataStruct {
                fields: Fields::Named(named),
                ..
            }) => named,
            _ => {
                return Err(Error::new_spanned(
                    input,
                    "CustomDebug only works on structures with named fields.",
                ))
            }
        };
        let st_name = &input.ident;
        let str_st_name = &input.ident.to_string();
        let custom_debug_field_calls = custom_debug_field_calls(&st_fields.named)?;

        Ok(quote! {
            #[automatically_derived]
            impl #impl_generics ::std::fmt::Debug for #st_name #ty_generics #where_clause {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    f.debug_struct(#str_st_name)
                        #(#custom_debug_field_calls)*
                        .finish()
                }
            }
        })
    }

    fn add_trait_bounds(mut generics: Generics) -> Generics {
        for param in &mut generics.params {
            if let GenericParam::Type(ref mut type_param) = *param {
                type_param.bounds.push(parse_quote!(::std::fmt::Debug));
            }
        }
        generics
    }

    fn custom_debug_field_calls(fields: &Punctuated<Field, Comma>) -> Result<Vec<TokenStream>> {
        fields
            .iter()
            .map(|f| {
                let f_name = &f.ident;
                let str_f_name = (&f.ident)
                    .as_ref()
                    .ok_or_else(|| Error::new_spanned(f, "Fields must be named"))?
                    .to_string();
                if let Some(format_str) = extract_debug_attribute(&f.attrs)? {
                    Ok(quote! {
                        .field(#str_f_name, &format_args!("{:?}", format_args!(#format_str , &self.#f_name)))
                    })
                } else {
                    Ok(quote! {
                        .field(#str_f_name, &self.#f_name)
                    })
                }
            })
            .collect()
    }

    fn extract_debug_attribute(attrs: &[Attribute]) -> Result<Option<String>> {
        attrs
            .iter()
            .find_map(|att| match att.parse_meta() {
                Ok(meta) => match &meta {
                    syn::Meta::NameValue(pair) => {
                        if pair.path.segments.len() != 1 || pair.path.segments[0].ident != "debug" {
                            Some(Err(Error::new_spanned(
                                meta,
                                "Only `debug = \"format_str\"` is supported",
                            )))
                        } else if let Lit::Str(ref lit_str) = pair.lit {
                            Some(Ok(lit_str.value()))
                        } else {
                            Some(Err(Error::new_spanned(
                                meta,
                                "Only `debug = \"format_str\"` is supported",
                            )))
                        }
                    }
                    _ => Some(Err(Error::new_spanned(
                        meta,
                        "Only `debug = \"format_str\"` is supported",
                    ))),
                },
                Err(e) => Some(Err(Error::new_spanned(
                    att,
                    format!("could not parse the meta attribute: {e}"),
                ))),
            })
            .transpose()
    }
}
