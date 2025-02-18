extern crate proc_macro;
extern crate alloc;

#[macro_use]
mod shared;

mod serde_bin;
use crate::serde_bin::*;

mod serde_ron;
use crate::serde_ron::*;

mod serde_json;

mod parse;

use crate::serde_json::*;


#[proc_macro_derive(SerBin, attributes(nserde))]
pub fn derive_ser_bin(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse::parse_data(input);

    if let Some(proxy) = shared::attrs_proxy(&input.attributes()) {
        return derive_ser_bin_proxy(&proxy, &input.name());
    }

    // ok we have an ident, its either a struct or a enum
    let ts = match &input {
        parse::Data::Struct(struct_) if struct_.named => derive_ser_bin_struct(struct_),
        parse::Data::Struct(struct_) => derive_ser_bin_struct_unnamed(struct_),
        parse::Data::Enum(enum_) => derive_ser_bin_enum(enum_),
        _ => unimplemented!("Only structs and enums are supported"),
    };

    ts
}

#[proc_macro_derive(DeBin, attributes(nserde))]
pub fn derive_de_bin(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse::parse_data(input);

    if let Some(proxy) = shared::attrs_proxy(&input.attributes()) {
        return derive_de_bin_proxy(&proxy, &input.name());
    }

    // ok we have an ident, its either a struct or a enum
    let ts = match &input {
        parse::Data::Struct(struct_) if struct_.named => derive_de_bin_struct(struct_),
        parse::Data::Struct(struct_) => derive_de_bin_struct_unnamed(struct_),
        parse::Data::Enum(enum_) => derive_de_bin_enum(enum_),

        _ => unimplemented!("Only structs and enums are supported"),
    };

    ts
}

#[proc_macro_derive(SerRon, attributes(nserde))]
pub fn derive_ser_ron(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse::parse_data(input);

    if let Some(proxy) = shared::attrs_proxy(&input.attributes()) {
        return derive_ser_ron_proxy(&proxy, &input.name());
    }

    // ok we have an ident, its either a struct or a enum
    let ts = match &input {
        parse::Data::Struct(struct_) if struct_.named => derive_ser_ron_struct(struct_),
        parse::Data::Struct(struct_) => derive_ser_ron_struct_unnamed(struct_),
        parse::Data::Enum(enum_) => derive_ser_ron_enum(enum_),
        _ => unimplemented!("Only structs and enums are supported"),
    };

    ts
}

#[proc_macro_derive(DeRon, attributes(nserde))]
pub fn derive_de_ron(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse::parse_data(input);

    if let Some(proxy) = shared::attrs_proxy(&input.attributes()) {
        return derive_de_ron_proxy(&proxy, &input.name());
    }

    // ok we have an ident, its either a struct or a enum
    let ts = match &input {
        parse::Data::Struct(struct_) if struct_.named => derive_de_ron_struct(struct_),
        parse::Data::Struct(struct_) => derive_de_ron_struct_unnamed(struct_),
        parse::Data::Enum(enum_) => derive_de_ron_enum(enum_),
        _ => unimplemented!("Only structs and enums are supported"),
    };

    ts
}



#[proc_macro_derive(ToJSON, attributes(nserde))]
pub fn derive_ser_json(input_ts: proc_macro::TokenStream) -> proc_macro::TokenStream {

    /*let input = parse::parse_data(input_ts.clone());

    if let Some(proxy) = shared::attrs_proxy(&input.attributes()) {
        return derive_ser_json_proxy(&proxy, &input.name());
    }
    */

    let new_input_ts = input_ts.clone();

    let input = syn::parse_macro_input!(input_ts as syn::DeriveInput);

    match &input.data {
        syn::Data::Struct(_) => derive_ser_json_struct(new_input_ts),
        /*
        syn::Data::Struct(struct_) => { 
                                    if let parse::Data::Struct(struct_) = parse::parse_data(input_ts) {
                                            derive_ser_json_struct_unnamed(&struct_)
                                    }else {
                                        proc_macro::TokenStream::new()
                                    }
                                },
        */
        syn::Data::Enum(enum_) => {
                                if let parse::Data::Enum(enum_) = parse::parse_data(new_input_ts) {
                                    derive_ser_json_enum(&enum_)
                                } else {
                                    proc_macro::TokenStream::new()
                                }
                                
                            }
        _ => unimplemented!(""),
    }

}





#[proc_macro_derive(DeJson, attributes(nserde))]
pub fn derive_de_json(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse::parse_data(input);

    if let Some(proxy) = shared::attrs_proxy(&input.attributes()) {
        return derive_de_json_proxy(&proxy, &input.name());
    }
    // ok we have an ident, its either a struct or a enum
    let ts = match &input {
        parse::Data::Struct(struct_) if struct_.named => derive_de_json_struct(struct_),
        parse::Data::Struct(struct_) => derive_de_json_struct_unnamed(struct_),
        parse::Data::Enum(enum_) => derive_de_json_enum(enum_),
        parse::Data::Union(_) => unimplemented!("Unions are not supported"),
    };

    ts
}
