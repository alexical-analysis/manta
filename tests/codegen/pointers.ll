; ModuleID = 'pointers'
source_filename = "pointers"

define void @"<init>"() {
entry:
  ret void
}

define void @fmt_println(i64 %0) {
entry:
  %i = alloca i64, align 8
  ret void
}

define void @main() {
entry:
  %i = alloca i64, align 8
  %j = alloca i64, align 8
  %p = alloca ptr, align 8
  %deref = load ptr, ptr %p, align 8
  %load = load i64, ptr %deref, align 8
  %load1 = load i64, ptr %i, align 8
  %deref2 = load ptr, ptr %p, align 8
  %load3 = load i64, ptr %deref2, align 8
  %deref4 = load ptr, ptr %p, align 8
  %load5 = load i64, ptr %deref4, align 8
  %sdiv = sdiv i64 %load5, 37
  %load6 = load i64, ptr %j, align 8
  ret void
}
