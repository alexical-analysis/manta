use std::collections::{BTreeMap, HashSet};

use serde::Serialize;

use crate::{
    hir::{self, NodeID},
    mir::{TagSize, TypeSpec, TypeValue},
};

#[derive(Serialize)]
pub struct TypeContext {
    values: Vec<TypeValue>, // Indexed by TypeSpec
    // core types tracks type specs that can not be modified by user code (e.g., u32, i8, bool)
    // this can be used to prevent repeated insertions of the same type into the context, though
    // it's up to the builder to ensure this as the context will not prevent duplicate types
    core_types: BTreeMap<TypeValue, TypeSpec>,
}

impl TypeContext {
    pub fn new() -> Self {
        TypeContext {
            values: vec![],
            core_types: BTreeMap::new(),
        }
    }

    fn add_type_value(&mut self, type_value: TypeValue) -> TypeSpec {
        let id = self.values.len();
        self.values.push(type_value);

        TypeSpec::from(id)
    }

    fn set_type_value(&mut self, type_spec: TypeSpec, type_value: TypeValue) {
        match self.values.get_mut(type_spec.idx()) {
            Some(v) => *v = type_value,
            None => panic!("unkonwn type spec"),
        }
    }

    fn core_type_id(&mut self, type_value: TypeValue) -> TypeSpec {
        match self.core_types.get(&type_value) {
            Some(type_spec) => *type_spec,
            None => {
                let type_spec = self.add_type_value(type_value.clone());

                // make sure we track the newly added core type so we don't add it again in the future
                self.core_types.insert(type_value, type_spec);
                type_spec
            }
        }
    }

    pub fn type_i8(&mut self) -> TypeSpec {
        self.core_type_id(TypeValue::I8)
    }

    pub fn type_i16(&mut self) -> TypeSpec {
        self.core_type_id(TypeValue::I16)
    }

    pub fn type_i32(&mut self) -> TypeSpec {
        self.core_type_id(TypeValue::I32)
    }

    pub fn type_i64(&mut self) -> TypeSpec {
        self.core_type_id(TypeValue::I64)
    }

    pub fn type_f32(&mut self) -> TypeSpec {
        self.core_type_id(TypeValue::F32)
    }

    pub fn type_f64(&mut self) -> TypeSpec {
        self.core_type_id(TypeValue::F64)
    }

    pub fn type_bool(&mut self) -> TypeSpec {
        self.core_type_id(TypeValue::Bool)
    }

    pub fn type_str(&mut self) -> TypeSpec {
        self.core_type_id(TypeValue::String)
    }

    pub fn type_opaque_ptr(&mut self) -> TypeSpec {
        self.core_type_id(TypeValue::OpaquePtr)
    }

    pub fn type_unit(&mut self) -> TypeSpec {
        self.core_type_id(TypeValue::Unit)
    }

    pub fn is_signed_type(&self, type_spec: TypeSpec) -> bool {
        match self.values.get(type_spec.idx()) {
            Some(v) => matches!(
                *v,
                TypeValue::I8
                    | TypeValue::I16
                    | TypeValue::I32
                    | TypeValue::I64
                    | TypeValue::F32
                    | TypeValue::F64
            ),
            None => panic!("unknown type spec"),
        }
    }

    pub fn is_unit_type(&mut self, type_spec: TypeSpec) -> bool {
        match self.values.get(type_spec.idx()) {
            Some(v) => *v == TypeValue::Unit,
            None => panic!("unknown type spec"),
        }
    }

    fn new_builder<'ctx>(&'ctx mut self) -> TypeBuilder<'ctx> {
        TypeBuilder {
            ctx: self,
            node_map: BTreeMap::new(),
            unfinished_types: HashSet::new(),
        }
    }
}

pub struct TypeBuilder<'ctx> {
    ctx: &'ctx mut TypeContext,
    node_map: BTreeMap<NodeID, TypeSpec>,

    // track unfinshed types to make sure we fill in all the type holes
    unfinished_types: HashSet<NodeID>,
}

impl<'ctx> TypeBuilder<'ctx> {
    fn get_node_type_spec(&mut self, node_id: NodeID) -> Option<TypeSpec> {
        self.node_map.get(&node_id).copied()
    }

    fn add_unknown_node(&mut self, node_id: NodeID) -> TypeSpec {
        // this can not be a ctx.type_unit() call because we need to be able to replace this type durring
        // the building process
        let type_spec = self.ctx.add_type_value(TypeValue::Unit);

        self.node_map.insert(node_id, type_spec);
        self.unfinished_types.insert(node_id);

        type_spec
    }

    pub fn get_type_spec(&mut self, node_id: NodeID, hir_type: hir::TypeSpec) -> TypeSpec {
        match self.unfinished_types.contains(&node_id) {
            true => {
                let type_spec = *self
                    .node_map
                    .get(&node_id)
                    .expect("failed to find type spec");

                let type_value = self.lower_type_value(hir_type);
                self.ctx.set_type_value(type_spec, type_value);
                self.unfinished_types.remove(&node_id);

                type_spec
            }
            false => {
                debug_assert!(
                    !self.node_map.contains_key(&node_id),
                    "node map already contains a mapping for this type"
                );

                self.lower_type_spec(hir_type)
            }
        }
    }

    fn add_type_value(&mut self, type_value: TypeValue) -> TypeSpec {
        self.ctx.add_type_value(type_value)
    }

    fn lower_type_spec(&mut self, hir_type: hir::TypeSpec) -> TypeSpec {
        match hir_type {
            hir::TypeSpec::Int8 => self.ctx.type_i8(),
            hir::TypeSpec::Int16 => self.ctx.type_i16(),
            hir::TypeSpec::Int32 => self.ctx.type_i32(),
            hir::TypeSpec::Int64 => self.ctx.type_i64(),
            hir::TypeSpec::UInt8 => self.ctx.type_i8(),
            hir::TypeSpec::UInt16 => self.ctx.type_i16(),
            hir::TypeSpec::UInt32 => self.ctx.type_i32(),
            hir::TypeSpec::UInt64 => self.ctx.type_i64(),
            hir::TypeSpec::Float32 => self.ctx.type_f32(),
            hir::TypeSpec::Float64 => self.ctx.type_f64(),
            hir::TypeSpec::String => self.ctx.type_str(),
            hir::TypeSpec::Bool => self.ctx.type_bool(),
            hir::TypeSpec::UnsafePtr => self.ctx.type_opaque_ptr(),
            hir::TypeSpec::Panic => self.ctx.type_unit(),
            hir::TypeSpec::Unit => self.ctx.type_unit(),
            hir_type => {
                let type_value = self.lower_type_value(hir_type);
                self.add_type_value(type_value)
            }
        }
    }

    fn lower_type_value(&mut self, hir_type: hir::TypeSpec) -> TypeValue {
        match hir_type {
            hir::TypeSpec::Int8 => TypeValue::I8,
            hir::TypeSpec::Int16 => TypeValue::I16,
            hir::TypeSpec::Int32 => TypeValue::I32,
            hir::TypeSpec::Int64 => TypeValue::I32,
            hir::TypeSpec::UInt8 => TypeValue::I8,
            hir::TypeSpec::UInt16 => TypeValue::I16,
            hir::TypeSpec::UInt32 => TypeValue::I32,
            hir::TypeSpec::UInt64 => TypeValue::I64,
            hir::TypeSpec::Float32 => TypeValue::F32,
            hir::TypeSpec::Float64 => TypeValue::F64,
            hir::TypeSpec::String => TypeValue::String,
            hir::TypeSpec::Bool => TypeValue::Bool,
            hir::TypeSpec::UnsafePtr => TypeValue::OpaquePtr,
            hir::TypeSpec::Panic => TypeValue::Unit,
            hir::TypeSpec::Unit => TypeValue::Unit,
            hir::TypeSpec::Named(type_spec) => match self.node_map.get(&type_spec.name) {
                Some(inner) => TypeValue::Named(*inner),
                None => {
                    let inner = self.add_unknown_node(type_spec.name);
                    TypeValue::Named(inner)
                }
            },
            hir::TypeSpec::Pointer(type_spec) => {
                let inner = self.lower_type_spec(*type_spec);
                TypeValue::Ptr(inner)
            }
            hir::TypeSpec::Slice(type_spec) => {
                let inner = self.lower_type_spec(*type_spec);
                TypeValue::Slice(inner)
            }
            hir::TypeSpec::Array(type_spec) => {
                let elem = self.lower_type_spec(*type_spec.type_spec);
                TypeValue::Array {
                    elem,
                    len: type_spec.size,
                }
            }
            hir::TypeSpec::Struct(type_spec) => {
                let fields = type_spec
                    .fields
                    .iter()
                    .map(|f| self.lower_type_spec(f.type_spec.clone()))
                    .collect();

                TypeValue::Struct(fields)
            }
            hir::TypeSpec::Enum(type_spec) => {
                let variants = type_spec
                    .variants
                    .iter()
                    .map(|v| match v.payload.as_ref() {
                        Some(ts) => self.lower_type_spec(ts.clone()),
                        None => self.ctx.type_unit(),
                    })
                    .collect();

                TypeValue::Enum {
                    tag_size: tag_size_for(type_spec.variants.len()),
                    variants,
                }
            }
            hir::TypeSpec::Function(type_spec) => self.lower_type_value(*type_spec.return_type),
            hir::TypeSpec::IntLiteral(_) => {
                panic!("int literal types not allowed in blocker type lowering")
            }
            hir::TypeSpec::UIntLiteral(_) => {
                panic!("uint literal types not allowed in blocker type lowering")
            }
            hir::TypeSpec::FloatLiteral(_) => {
                panic!("float literal types not allowed in blocker type lowering")
            }
            hir::TypeSpec::InferredEnumExpr(_) => {
                panic!("inferred enum expression types not allowed in blocker type lowering")
            }
            hir::TypeSpec::InferredEnumPat(_) => {
                panic!("inferred enum pattern types not allowed in blocker type lowering")
            }
            hir::TypeSpec::Any => panic!("internal any type not allowed in blocker type lowering"),
        }
    }
}

fn tag_size_for(variant_count: usize) -> TagSize {
    if variant_count <= u8::MAX as usize + 1 {
        TagSize::U8
    } else if variant_count <= u16::MAX as usize + 1 {
        TagSize::U16
    } else if variant_count <= u32::MAX as usize + 1 {
        TagSize::U32
    } else {
        TagSize::U64
    }
}
