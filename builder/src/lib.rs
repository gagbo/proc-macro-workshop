use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    builder_derive::expand(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

mod builder_derive {

    use proc_macro2::Span;
    use proc_macro2::TokenStream;
    use quote::quote;
    use syn::punctuated::Punctuated;
    use syn::spanned::Spanned;
    use syn::token::Comma;
    use syn::Field;
    use syn::{
        Attribute, Data, DataStruct, DeriveInput, Error, Fields, GenericArgument, Ident, Lit, Meta,
        Path, PathArguments, Result, Type,
    };

    const BUILDER_ALLOWED_ATTRIBUTES: &[&str; 1] = &["each"];

    pub(crate) fn expand(input: DeriveInput) -> Result<TokenStream> {
        let st_name = input.ident;
        let builder_name = Ident::new(&format!("{st_name}Builder"), Span::call_site());
        let fields = match input.data {
            Data::Struct(DataStruct {
                fields: Fields::Named(fields),
                ..
            }) => fields.named,
            _ => {
                return Err(Error::new(
                    st_name.span(),
                    "The macro only works on named structs",
                ))
            }
        };

        let builder_struct_fields = builder_struct_fields(&fields)?;

        let builder_struct_initializers = builder_struct_initializers(&fields)?;

        let builder_setter_methods = builder_setter_methods(&fields)?;

        let builder_field_builders = builder_field_builders(&fields)?;

        Ok(quote! {
            #[automatically_derived]
            impl #st_name {
                pub fn builder() -> #builder_name {
                    #builder_name {
                        #(#builder_struct_initializers),*
                    }
                }
            }

            #[automatically_derived]
            pub struct #builder_name {
                #(#builder_struct_fields),*
            }

            #[automatically_derived]
            impl #builder_name {
                #(#builder_setter_methods)*

                pub fn build(&mut self) -> ::std::result::Result<#st_name, ::std::boxed::Box<dyn ::std::error::Error>> {
                    Ok(
                        #st_name {
                            #(#builder_field_builders),*
                        }
                    )
                }
            }
        })
    }

    fn builder_struct_fields(fields: &Punctuated<Field, Comma>) -> Result<Vec<TokenStream>> {
        fields
            .iter()
            .map(|f| {
                let name = &f.ident;
                let ty = &f.ty;
                let attrs = &f.attrs;

                if builder_each_ident(attrs.as_slice())?.is_some() {
                    Ok(quote! {
                        #name: #ty
                    })
                } else {
                    Ok(quote! {
                        #name: ::std::option::Option<#ty>
                    })
                }
            })
            .collect()
    }

    fn builder_struct_initializers(fields: &Punctuated<Field, Comma>) -> Result<Vec<TokenStream>> {
        fields
            .iter()
            .map(|f| {
                let name = &f.ident;
                let attrs = &f.attrs;

                if builder_each_ident(attrs.as_slice())?.is_some() {
                    Ok(quote! {
                        #name: ::std::vec::Vec::new()
                    })
                } else {
                    Ok(quote! {
                        #name: None
                    })
                }
            })
            .collect()
    }

    fn builder_setter_methods(fields: &Punctuated<Field, Comma>) -> Result<Vec<TokenStream>> {
        fields
            .iter()
            .map(|f| {
                let name = &f.ident;
                let ty = &f.ty;
                let attrs = &f.attrs;

                if let Some(each_name) = builder_each_ident(attrs.as_slice())? {
                    match ty {
                        Type::Path(type_path) => {
                            if let Some(inner_ty) = vec_inner_type(&type_path.path) {
                                Ok(quote! {
                                    fn #each_name(&mut self, #each_name: #inner_ty) -> &mut Self {
                                        self.#name.push(#each_name);
                                        self
                                    }
                                })
                            } else {
                                Err(Error::new(f.span(), "'each' attribute must match a Vec"))
                            }
                        }
                        _ => Err(Error::new(f.span(), "'each' attribute must match a Vec")),
                    }
                } else {
                    match ty {
                        Type::Path(type_path) => {
                            if let Some(inner_ty) = option_inner_type(&type_path.path) {
                                Ok(quote! {
                                    fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                                        self.#name = Some(Some(#name));
                                        self
                                    }
                                })
                            } else {
                                Ok(quote! {
                                    fn #name(&mut self, #name: #ty) -> &mut Self {
                                        self.#name = Some(#name);
                                        self
                                    }
                                })
                            }
                        }
                        _ => Ok(quote! {
                            fn #name(&mut self, #name: #ty) -> &mut Self {
                                self.#name = Some(#name);
                                self
                            }
                        }),
                    }
                }
            })
            .collect()
    }

    fn builder_field_builders(fields: &Punctuated<Field, Comma>) -> Result<Vec<TokenStream>> {
        fields.iter().map(|f| {
        let name = &f.ident;
        let str_name = format!("{:?}", name);
        let ty = &f.ty;
        let attrs = &f.attrs;

        if builder_each_ident(attrs.as_slice())?.is_some() {
            Ok(quote! {
                #name: self.#name.clone()
            })
        } else {
            match ty {
                Type::Path(type_path) => {
                    if option_inner_type(&type_path.path).is_some() {
                        Ok(quote! {
                            #name: self.#name.clone().unwrap_or_default()
                        })
                    } else {
                        Ok(quote! {
                            #name: self.#name.clone().ok_or_else(|| format!("{:?} missing", #str_name))?
                        })
                    }
                }
                _ => {
                    Ok(quote! {
                        #name: self.#name.clone().ok_or_else(|| format!("{:?} missing", #str_name))?
                    })
                }
            }
        }
    })
                     .collect()
    }

    fn builder_each_ident(attrs: &[Attribute]) -> Result<Option<Ident>> {
        attrs
            .iter()
            .find_map(|att| match att.parse_meta() {
                Ok(Meta::List(list)) => {
                    if list.path.segments.last().unwrap().ident != "builder" {
                        None
                    } else {
                        list.nested
                            .iter()
                            .find_map(|elem| match elem {
                                syn::NestedMeta::Meta(Meta::NameValue(pair)) => {
                                    if pair.path.segments.len() == 1
                                        && pair.path.segments[0].ident == "each"
                                    {
                                        match pair.lit {
                                            Lit::Str(ref name) => Some(Ok(name.value())),
                                            _ => Some(Err(Error::new(
                                                pair.lit.span(),
                                                "Only string literals are allowed for 'each'",
                                            ))),
                                        }
                                    } else if !BUILDER_ALLOWED_ATTRIBUTES
                                        .contains(&pair.path.segments[0].ident.to_string().as_str())
                                    {
                                        Some(Err(Error::new_spanned(
                                            list.clone(),
                                            "expected `builder(each = \"...\")`",
                                        )))
                                    } else {
                                        None
                                    }
                                }
                                _ => None,
                            })
                            .map(|res| res.map(|each_n| Ident::new(&each_n, Span::call_site())))
                    }
                }
                _ => None,
            })
            .transpose()
    }

    fn vec_inner_type(path: &Path) -> Option<&Type> {
        generic_inner_type(path, "Vec")
    }

    fn option_inner_type(path: &Path) -> Option<&Type> {
        generic_inner_type(path, "Option")
    }

    fn generic_inner_type<'a>(path: &'a Path, generic: &str) -> Option<&'a Type> {
        if path.leading_colon.is_some() {
            return None;
        }

        if path.segments.len() != 1 || path.segments[0].ident != generic {
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
