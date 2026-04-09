; ModuleID = 'enum_polymorphism'
source_filename = "enum_polymorphism"

@MATH_PI = external global double

define void @"<init>"() {
entry:
  store double 3.140000e+00, ptr @MATH_PI, align 8
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
  %load = load { i8, [8 x i8] }, ptr %s, align 1
  switch i1 false, label %Block_7 [
    i8 0, label %Block_3
    i8 1, label %Block_5
  ]
  %load2 = load double, ptr @MATH_PI, align 8
  %load3 = load double, ptr %r, align 8
  %fmul = fmul double %load2, %load3
  %load4 = load double, ptr %r, align 8
  %fmul5 = fmul double %fmul, %load4
  ret double %fmul5
  %load6 = load double, ptr %s1, align 8
  %load7 = load double, ptr %s1, align 8
  %fmul8 = fmul double %load6, %load7
  ret double %fmul8
  unreachable

Block_3:                                          ; preds = %entry

Block_5:                                          ; preds = %entry

Block_7:                                          ; preds = %entry
}

define void @main() {
entry:
  %square = alloca { i8, [8 x i8] }, align 8
  %square_area = alloca double, align 8
  %circle = alloca { i8, [8 x i8] }, align 8
  %circle_area = alloca double, align 8
  %load = load { i8, [8 x i8] }, ptr %square, align 1
  %load1 = load double, ptr %square_area, align 8
  %load2 = load { i8, [8 x i8] }, ptr %circle, align 1
  %load3 = load double, ptr %circle_area, align 8
  ret void
}
