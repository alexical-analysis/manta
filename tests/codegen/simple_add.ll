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
  store i64 10, ptr %a, align 8
  store i64 20, ptr %b, align 8
  store i64 5, ptr %c, align 8
  %load = load i64, ptr %a, align 8
  %load1 = load i64, ptr %b, align 8
  %iadd = add i64 %load, %load1
  %load2 = load i64, ptr %c, align 8
  %isub = sub i64 %iadd, %load2
  ret i64 %isub
}
