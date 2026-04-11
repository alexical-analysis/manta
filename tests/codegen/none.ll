; ModuleID = 'none'
source_filename = "none"

@panic_msg = private unnamed_addr constant [24 x i8] c"Panic reached! exiting!\00", align 1

define void @"<init>"() {
entry:
  ret void
}

define void @fmt_println_ptr(ptr %0) {
entry:
  %p = alloca ptr, align 8
  ret void
}

define void @fmt_println({ i64, ptr } %0) {
entry:
  %s = alloca { i64, ptr }, align 8
  ret void
}

define { i8, [8 x i8] } @maybe_alloc(i1 %0) {
entry:
  %cond = alloca i1, align 1
  %p = alloca ptr, align 8
  %p1 = alloca ptr, align 8
  %panic = alloca ptr, align 8
  %load = load i1, ptr %cond, align 1
  br i1 %load, label %Block_2, label %Block_3

Block_2:                                          ; preds = %entry
  br i1 false, label %Block_5, label %Block_7

Block_3:                                          ; preds = %entry
  unreachable

Block_4:                                          ; preds = %Block_6
  %load2 = load ptr, ptr %p, align 8
  store i32 42, ptr %load2, align 4
  %load3 = load ptr, ptr %p, align 8
  unreachable

Block_5:                                          ; preds = %Block_2
  %load4 = load ptr, ptr %p1, align 8
  store ptr %load4, ptr %p, align 8
  br label %Block_6

Block_6:                                          ; preds = %Block_5
  br label %Block_4

Block_7:                                          ; preds = %Block_2
  call void @panic()
  unreachable
}

define void @main() {
entry:
  %"<match target>" = alloca { i8, [8 x i8] }, align 8
  %p = alloca ptr, align 8
  %maybe_alloc = call { i8, [8 x i8] } @maybe_alloc(i1 false)
  store { i8, [8 x i8] } %maybe_alloc, ptr %"<match target>", align 1
  %ext_tag = extractvalue { i8, [8 x i8] } %maybe_alloc, 0
  switch i8 %ext_tag, label %Block_7 [
    i8 0, label %Block_3
    i8 1, label %Block_5
  ]

Block_2:                                          ; preds = %Block_6, %Block_4
  ret void

Block_3:                                          ; preds = %entry
  %ext_pay = getelementptr inbounds nuw { i8, [8 x i8] }, ptr %"<match target>", i32 0, i32 1
  %load = load ptr, ptr %ext_pay, align 8
  store ptr %load, ptr %p, align 8
  %load1 = load ptr, ptr %p, align 8
  call void @fmt_println_ptr(ptr %load1)
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
