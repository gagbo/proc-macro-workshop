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
        DeriveInput, Error, Field, Fields, GenericArgument, GenericParam, Generics, Lit, Path,
        PathArguments, PredicateType, Result, Token, TraitBound, Type, TypeParam, TypeParamBound,
        TypePath, WherePredicate,
    };

    pub(crate) fn expand(input: DeriveInput) -> Result<TokenStream> {
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

        let generics = add_trait_bounds(input.generics.clone(), &st_fields.named);
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

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

    fn add_trait_bounds(mut generics: Generics, fields: &Punctuated<Field, Comma>) -> Generics {
        let mut additional_params = Vec::new();
        for param in &mut generics.params {
            if let GenericParam::Type(ref mut type_param) = *param {
                list_associated_and_inner_types_for_bounds(fields, type_param)
                    .into_iter()
                    .for_each(|assoc_ty| {
                        #[allow(clippy::useless_conversion)]
                        let bounds = vec![TypeParamBound::from(TraitBound::from(parse_quote!(
                            ::std::fmt::Debug
                        )))]
                        .into_iter()
                        .collect();

                        let assoc_type_clause = WherePredicate::Type(PredicateType {
                            lifetimes: None,
                            bounded_ty: assoc_ty,
                            colon_token: <Token![:]>::default(),
                            bounds,
                        });
                        additional_params.push(assoc_type_clause);
                    });
            }
        }

        generics
            .make_where_clause()
            .predicates
            .extend(additional_params.into_iter());

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

    fn list_associated_and_inner_types_for_bounds(
        fields: &Punctuated<Field, Comma>,
        type_param: &TypeParam,
    ) -> Vec<Type> {
        fn assoc_types_in_type(ty: &Type, result: &mut Vec<Type>, type_param: &TypeParam) {
            if let Type::Path(TypePath { qself: _, ref path }) = ty {
                if path.segments.len() > 1 && path.segments[0].ident == type_param.ident {
                    result.push(ty.clone())
                } else if pd_inner_type(path).is_some() {
                    // Stop the recursion if we meet the PhantomData type
                } else if path.segments.len() == 1 && path.segments[0].ident == type_param.ident {
                    // Push the type itself if it appears in raw form
                    result.push(ty.clone())
                } else if let Some(inner) = generic_inner_type(path, None) {
                    assoc_types_in_type(inner, result, type_param)
                } else {
                    // Nothing
                }
            }
        }

        fields
            .iter()
            .map(|f| {
                let mut inner_res = Vec::new();
                assoc_types_in_type(&f.ty, &mut inner_res, type_param);
                inner_res
            })
            .flatten()
            .collect()
    }

    fn pd_inner_type(path: &Path) -> Option<&Type> {
        generic_inner_type(path, Some("PhantomData"))
    }

    fn generic_inner_type<'a>(path: &'a Path, generic: Option<&str>) -> Option<&'a Type> {
        if path.leading_colon.is_some() {
            return None;
        }

        if path.segments.len() != 1 || generic.map_or(false, |gen| path.segments[0].ident != gen) {
            return None;
        }

        let ab = match &path.segments[0].arguments {
            PathArguments::AngleBracketed(ab) => ab,
            _ => return None,
        };

        if ab.args.len() != 1 {
            return None;
        }

        match &ab.args[0] {
            GenericArgument::Type(t) => Some(t),
            _ => None,
        }
    }
}
