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
  store i64 42, ptr %i, align 8
  store i64 2701, ptr %j, align 8
  %deref = load ptr, ptr %p, align 8
  %load = load i64, ptr %deref, align 8
  %deref1 = load ptr, ptr %p, align 8
  store i64 21, ptr %deref1, align 8
  %load2 = load i64, ptr %i, align 8
  %deref3 = load ptr, ptr %p, align 8
  %load4 = load i64, ptr %deref3, align 8
  %deref5 = load ptr, ptr %p, align 8
  %load6 = load i64, ptr %deref5, align 8
  %sdiv = sdiv i64 %load6, 37
  %deref7 = load ptr, ptr %p, align 8
  %deref8 = load ptr, ptr %p, align 8
  %load9 = load i64, ptr %deref8, align 8
  %sdiv10 = sdiv i64 %load9, 37
  store i64 %sdiv10, ptr %deref7, align 8
  %load11 = load i64, ptr %j, align 8
  ret void
}
