; ModuleID = 'simple_add'
source_filename = "simple_add"

define void @"<init>"() {
entry:
  ret void
}

define i64 @main() {
entry:
  %a = alloca i64, align 8
  %b = alloca i64, align 8
  %c = alloca i64, align 8
  unreachable
}
