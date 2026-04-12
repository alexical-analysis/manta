; ModuleID = 'defer_free'
source_filename = "defer_free"

@panic_msg = private unnamed_addr constant [24 x i8] c"Panic reached! exiting!\00", align 1
@const_str = private constant [1 x i8] c"_"

define void @"<init>"() {
entry:
  ret void
}

declare void @print({ i64, ptr })

declare void @eprint({ i64, ptr })

define { i8, [0 x i8] } @os_open({ i64, ptr } %0) {
entry:
  %path = alloca { i64, ptr }, align 8
  store { i64, ptr } %0, ptr %path, align 8
  ret { i8, [0 x i8] } { i8 1, [0 x i8] undef }
}

define void @os_close({} %0) {
entry:
  %f = alloca {}, align 8
  store {} %0, ptr %f, align 1
  ret void
}

define { i8, [8 x i8] } @os_write({} %0, { i64, i64, ptr } %1) {
entry:
  %f = alloca {}, align 8
  %buf = alloca { i64, i64, ptr }, align 8
  store {} %0, ptr %f, align 1
  store { i64, i64, ptr } %1, ptr %buf, align 8
  ret { i8, [8 x i8] } { i8 1, [8 x i8] undef }
}

define { i8, [16 x i8] } @write_and_cleanup({ i64, ptr } %0) {
entry:
  %tmp21 = alloca [16 x i8], align 1
  %tmp = alloca [16 x i8], align 1
  %path = alloca { i64, ptr }, align 8
  %f = alloca {}, align 8
  %"<match target>" = alloca { i8, [0 x i8] }, align 8
  %f1 = alloca {}, align 8
  %"<wrap>" = alloca { i8, [0 x i8] }, align 8
  %buf = alloca ptr, align 8
  %buf2 = alloca ptr, align 8
  %panic = alloca ptr, align 8
  %n = alloca i64, align 8
  %"<match target>3" = alloca { i8, [8 x i8] }, align 8
  %n4 = alloca i64, align 8
  %"<wrap>5" = alloca { i8, [8 x i8] }, align 8
  %"<defer>" = alloca { i8, [16 x i8] }, align 8
  store { i64, ptr } %0, ptr %path, align 8
  %load = load { i64, ptr }, ptr %path, align 8
  %os_open = call { i8, [0 x i8] } @os_open({ i64, ptr } %load)
  store { i8, [0 x i8] } %os_open, ptr %"<match target>", align 1
  %ext_tag = extractvalue { i8, [0 x i8] } %os_open, 0
  switch i8 %ext_tag, label %Block_5 [
    i8 0, label %Block_3
  ]

Block_2:                                          ; preds = %Block_4
  %alloc = call ptr @alloc({ i64, i64, i64 } { i64 24, i64 8, i64 0 })
  %ptr_nonnull = icmp ne ptr %alloc, null
  br i1 %ptr_nonnull, label %Block_10, label %Block_12

Block_3:                                          ; preds = %entry
  %ext_pay = getelementptr inbounds nuw { i8, [0 x i8] }, ptr %"<match target>", i32 0, i32 1
  %load6 = load {}, ptr %ext_pay, align 1
  store {} %load6, ptr %f1, align 1
  %load7 = load {}, ptr %f1, align 1
  store {} %load7, ptr %f, align 1
  br label %Block_4

Block_4:                                          ; preds = %Block_3
  br label %Block_2

Block_5:                                          ; preds = %entry
  store { i8, [0 x i8] } %os_open, ptr %"<wrap>", align 1
  %load8 = load { i8, [0 x i8] }, ptr %"<wrap>", align 1
  store { i8, [0 x i8] } %load8, ptr %tmp, align 1
  %load9 = load [16 x i8], ptr %tmp, align 1
  %set_pay = insertvalue { i8, [16 x i8] } { i8 1, [16 x i8] undef }, [16 x i8] %load9, 1
  store { i8, [16 x i8] } %set_pay, ptr %"<defer>", align 1
  br label %Block_23

Block_7:                                          ; preds = %Block_15
  %load10 = load {}, ptr %f, align 1
  call void @os_close({} %load10)
  br label %Block_8

Block_8:                                          ; preds = %Block_7
  br label %Block_29

Block_9:                                          ; preds = %Block_11
  %load11 = load {}, ptr %f, align 1
  %load12 = load ptr, ptr %buf, align 8
  %load13 = load { i64, i64, ptr }, ptr %load12, align 8
  %os_write = call { i8, [8 x i8] } @os_write({} %load11, { i64, i64, ptr } %load13)
  store { i8, [8 x i8] } %os_write, ptr %"<match target>3", align 1
  %ext_tag14 = extractvalue { i8, [8 x i8] } %os_write, 0
  switch i8 %ext_tag14, label %Block_19 [
    i8 0, label %Block_17
  ]

Block_10:                                         ; preds = %Block_2
  store ptr %alloc, ptr %buf2, align 8
  %load15 = load ptr, ptr %buf2, align 8
  store ptr %load15, ptr %buf, align 8
  br label %Block_11

Block_11:                                         ; preds = %Block_10
  br label %Block_9

Block_12:                                         ; preds = %Block_2
  store ptr %alloc, ptr %panic, align 8
  br label %Block_14

Block_14:                                         ; preds = %Block_12
  %load16 = load ptr, ptr %buf, align 8
  call void @free(ptr %load16)
  br label %Block_15

Block_15:                                         ; preds = %Block_14
  br label %Block_7

Block_16:                                         ; preds = %Block_18
  store { i8, [16 x i8] } { i8 0, [16 x i8] undef }, ptr %"<defer>", align 1
  br label %Block_23

Block_17:                                         ; preds = %Block_9
  %ext_pay17 = getelementptr inbounds nuw { i8, [8 x i8] }, ptr %"<match target>3", i32 0, i32 1
  %load18 = load i64, ptr %ext_pay17, align 8
  store i64 %load18, ptr %n4, align 8
  %load19 = load i64, ptr %n4, align 8
  store i64 %load19, ptr %n, align 8
  br label %Block_18

Block_18:                                         ; preds = %Block_17
  br label %Block_16

Block_19:                                         ; preds = %Block_9
  store { i8, [8 x i8] } %os_write, ptr %"<wrap>5", align 1
  %load20 = load { i8, [8 x i8] }, ptr %"<wrap>5", align 1
  store { i8, [8 x i8] } %load20, ptr %tmp21, align 1
  %load22 = load [16 x i8], ptr %tmp21, align 1
  %set_pay23 = insertvalue { i8, [16 x i8] } { i8 2, [16 x i8] undef }, [16 x i8] %load22, 1
  store { i8, [16 x i8] } %set_pay23, ptr %"<defer>", align 1
  br label %Block_23

Block_23:                                         ; preds = %Block_19, %Block_16, %Block_5
  %load24 = load ptr, ptr %buf, align 8
  call void @free(ptr %load24)
  br label %Block_24

Block_24:                                         ; preds = %Block_23
  br label %Block_27

Block_27:                                         ; preds = %Block_24
  %load25 = load {}, ptr %f, align 1
  call void @os_close({} %load25)
  br label %Block_28

Block_28:                                         ; preds = %Block_27
  br label %Block_30

Block_29:                                         ; preds = %Block_8
  call void @panic()
  unreachable

Block_30:                                         ; preds = %Block_28
  %load26 = load { i8, [16 x i8] }, ptr %"<defer>", align 1
  ret { i8, [16 x i8] } %load26
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
