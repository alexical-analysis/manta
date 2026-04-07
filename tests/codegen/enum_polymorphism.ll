; ModuleID = 'enum_polymorphism'
source_filename = "enum_polymorphism"

@MATH_PI = external global double

define void @"<init>"() {
entry:
  ret void
}

define void @fmt_println(double %0) {
entry:
  %i = alloca double, align 8
  ret void
}

define double @area({ i8, [8 x i8] } %0) {
entry:
  %s = alloca { i8, [8 x i8] }, align 8
  %r = alloca double, align 8
  %s1 = alloca double, align 8
  switch i64 0, label %Block_7 [
    i8 0, label %Block_3
    i8 1, label %Block_5
  ]

Block_3:                                          ; preds = %entry
  unreachable

Block_5:                                          ; preds = %entry
  unreachable

Block_7:                                          ; preds = %entry
  unreachable
}

define void @main() {
entry:
  %square = alloca { i8, [8 x i8] }, align 8
  %square_area = alloca double, align 8
  %circle = alloca { i8, [8 x i8] }, align 8
  %circle_area = alloca double, align 8
  ret void
}
