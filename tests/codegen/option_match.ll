; ModuleID = 'option_match'
source_filename = "option_match"

@panic_msg = private unnamed_addr constant [24 x i8] c"Panic reached! exiting!\00", align 1

define void @"<init>"() {
entry:
  ret void
}

define void @fmt_println({ i64, ptr } %0) {
entry:
  %s = alloca { i64, ptr }, align 8
  ret void
}

define { i8, [4 x i8] } @div(i32 %0, i32 %1) {
entry:
  %tmp = alloca [4 x i8], align 1
  %a = alloca i32, align 4
  %b = alloca i32, align 4
  %load = load i32, ptr %b, align 4
  %ieq = icmp eq i32 %load, 0
  br i1 %ieq, label %Block_2, label %Block_3

Block_2:                                          ; preds = %entry
  ret { i8, [4 x i8] } { i8 1, [4 x i8] undef }

Block_3:                                          ; preds = %entry
  %load1 = load i32, ptr %a, align 4
  %load2 = load i32, ptr %b, align 4
  %sdiv = sdiv i32 %load1, %load2
  store i32 %sdiv, ptr %tmp, align 4
  %load3 = load [4 x i8], ptr %tmp, align 1
  %set_pay = insertvalue { i8, [4 x i8] } { i8 0, [4 x i8] undef }, [4 x i8] %load3, 1
  ret { i8, [4 x i8] } %set_pay
}

define void @main() {
entry:
  %r = alloca { i8, [4 x i8] }, align 8
  %v = alloca i32, align 4
  %div = call { i8, [4 x i8] } @div(i32 10, i32 2)
  store { i8, [4 x i8] } %div, ptr %r, align 1
  %load = load { i8, [4 x i8] }, ptr %r, align 1
  %ext_tag = extractvalue { i8, [4 x i8] } %load, 0
  switch i8 %ext_tag, label %Block_7 [
    i8 0, label %Block_3
    i8 1, label %Block_5
  ]

Block_2:                                          ; preds = %Block_6, %Block_4
  ret void

Block_3:                                          ; preds = %entry
  %ext_pay = getelementptr inbounds nuw { i8, [4 x i8] }, ptr %r, i32 0, i32 1
  %load1 = load i32, ptr %ext_pay, align 4
  store i32 %load1, ptr %v, align 4
  %load2 = load i32, ptr %v, align 4
  call void @fmt_println(i64 0, i32 %load2)
  br label %Block_4

Block_4:                                          ; preds = %Block_3
  br label %Block_2

Block_5:                                          ; preds = %entry
  call void @fmt_println(i64 0)
  br label %Block_6

Block_6:                                          ; preds = %Block_5
  br label %Block_2

Block_7:                                          ; preds = %entry
  unreachable
}

declare ptr @malloc(i64)

declare void @free(ptr)

declare i32 @puts(ptr)

declare void @abort()

define void @panic() {
entry:
  %puts = call i32 @puts(ptr @panic_msg)
  call void @abort()
  unreachable
}

define ptr @alloc({ i64, i64, i64 } %0) {
entry:
  %meta_size = extractvalue { i64, i64, i64 } %0, 0
  %malloc = call ptr @malloc(i64 %meta_size)
  ret ptr %malloc
}
