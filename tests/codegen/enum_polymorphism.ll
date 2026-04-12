; ModuleID = 'enum_polymorphism'
source_filename = "enum_polymorphism"

@MATH_PI = external global double
@panic_msg = private unnamed_addr constant [24 x i8] c"Panic reached! exiting!\00", align 1

define void @"<init>"() {
entry:
  store double 3.140000e+00, ptr @MATH_PI, align 8
  ret void
}

declare void @print({ i64, ptr })

declare void @eprint({ i64, ptr })

define void @fmt_println(double %0) {
entry:
  %i = alloca double, align 8
  store double %0, ptr %i, align 8
  ret void
}

define double @area({ i8, [8 x i8] } %0) {
entry:
  %s = alloca { i8, [8 x i8] }, align 8
  %r = alloca double, align 8
  %s1 = alloca double, align 8
  store { i8, [8 x i8] } %0, ptr %s, align 1
  %load = load { i8, [8 x i8] }, ptr %s, align 1
  %ext_tag = extractvalue { i8, [8 x i8] } %load, 0
  switch i8 %ext_tag, label %Block_7 [
    i8 0, label %Block_3
    i8 1, label %Block_5
  ]

Block_3:                                          ; preds = %entry
  %ext_pay = getelementptr inbounds nuw { i8, [8 x i8] }, ptr %s, i32 0, i32 1
  %load2 = load double, ptr %ext_pay, align 8
  store double %load2, ptr %r, align 8
  %load3 = load double, ptr @MATH_PI, align 8
  %load4 = load double, ptr %r, align 8
  %fmul = fmul double %load3, %load4
  %load5 = load double, ptr %r, align 8
  %fmul6 = fmul double %fmul, %load5
  ret double %fmul6

Block_5:                                          ; preds = %entry
  %ext_pay7 = getelementptr inbounds nuw { i8, [8 x i8] }, ptr %s, i32 0, i32 1
  %load8 = load double, ptr %ext_pay7, align 8
  store double %load8, ptr %s1, align 8
  %load9 = load double, ptr %s1, align 8
  %load10 = load double, ptr %s1, align 8
  %fmul11 = fmul double %load9, %load10
  ret double %fmul11

Block_7:                                          ; preds = %entry
  unreachable
}

define void @main() {
entry:
  %tmp3 = alloca [8 x i8], align 1
  %tmp = alloca [8 x i8], align 1
  %square = alloca { i8, [8 x i8] }, align 8
  %square_area = alloca double, align 8
  %circle = alloca { i8, [8 x i8] }, align 8
  %circle_area = alloca double, align 8
  store i64 0, ptr %tmp, align 8
  %load = load [8 x i8], ptr %tmp, align 1
  %set_pay = insertvalue { i8, [8 x i8] } { i8 1, [8 x i8] undef }, [8 x i8] %load, 1
  store { i8, [8 x i8] } %set_pay, ptr %square, align 1
  %load1 = load { i8, [8 x i8] }, ptr %square, align 1
  %area = call double @area({ i8, [8 x i8] } %load1)
  store double %area, ptr %square_area, align 8
  %load2 = load double, ptr %square_area, align 8
  call void @fmt_println(double %load2)
  store i64 0, ptr %tmp3, align 8
  %load4 = load [8 x i8], ptr %tmp3, align 1
  %set_pay5 = insertvalue { i8, [8 x i8] } { i8 0, [8 x i8] undef }, [8 x i8] %load4, 1
  store { i8, [8 x i8] } %set_pay5, ptr %circle, align 1
  %load6 = load { i8, [8 x i8] }, ptr %circle, align 1
  %area7 = call double @area({ i8, [8 x i8] } %load6)
  store double %area7, ptr %circle_area, align 8
  %load8 = load double, ptr %circle_area, align 8
  call void @fmt_println(double %load8)
  ret void
}

declare ptr @malloc(i64)

declare void @free(ptr)

declare i32 @puts(ptr)

declare void @abort()

declare i64 @write(i32, ptr, i64)

define internal void @panic() {
entry:
  %puts = call i32 @puts(ptr @panic_msg)
  call void @abort()
  unreachable
}

define internal ptr @alloc({ i64, i64, i64 } %0) {
entry:
  %meta_size = extractvalue { i64, i64, i64 } %0, 0
  %malloc = call ptr @malloc(i64 %meta_size)
  ret ptr %malloc
}

define internal void @print.1({ i64, ptr } %0) {
entry:
  %len = extractvalue { i64, ptr } %0, 0
  %ptr = extractvalue { i64, ptr } %0, 1
  %write = call i64 @write(i32 1, ptr %ptr, i64 %len)
  ret void

entry1:                                           ; No predecessors!
  %_ = alloca { i64, ptr }, align 8
  store { i64, ptr } %0, ptr %_, align 8
  ret void
}

define internal void @eprint.2({ i64, ptr } %0) {
entry:
  %len = extractvalue { i64, ptr } %0, 0
  %ptr = extractvalue { i64, ptr } %0, 1
  %write = call i64 @write(i32 2, ptr %ptr, i64 %len)
  ret void

entry1:                                           ; No predecessors!
  %_ = alloca { i64, ptr }, align 8
  store { i64, ptr } %0, ptr %_, align 8
  ret void
}
