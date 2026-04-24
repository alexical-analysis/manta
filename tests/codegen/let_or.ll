; ModuleID = 'let_or'
source_filename = "let_or"

@panic_msg = private unnamed_addr constant [24 x i8] c"Panic reached! exiting!\00", align 1
@const_str = private constant [10 x i8] c"/tmp/x.txt"
@const_str.1 = private constant [16 x i8] c"/tmp/missing.txt"
@const_str.2 = private constant [11 x i8] c"read failed"
@const_str.3 = private constant [1 x i8] c"_"

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

define internal void @print({ i64, ptr } %0) {
entry:
  %len = extractvalue { i64, ptr } %0, 0
  %ptr = extractvalue { i64, ptr } %0, 1
  %write = call i64 @write(i32 1, ptr %ptr, i64 %len)
  ret void
}

define internal void @eprint({ i64, ptr } %0) {
entry:
  %len = extractvalue { i64, ptr } %0, 0
  %ptr = extractvalue { i64, ptr } %0, 1
  %write = call i64 @write(i32 2, ptr %ptr, i64 %len)
  ret void
}

define void @"manta_<init>"() {
entry:
  ret void
}

define internal { i8, [0 x i8] } @os_open({ i64, ptr } %0) {
entry:
  %path = alloca { i64, ptr }, align 8
  store { i64, ptr } %0, ptr %path, align 8
  ret { i8, [0 x i8] } { i8 1, [0 x i8] undef }
}

define internal void @os_close({} %0) {
entry:
  %f = alloca {}, align 8
  store {} %0, ptr %f, align 1
  ret void
}

define internal { i8, [16 x i8] } @io_read_to_string({} %0) {
entry:
  %f = alloca {}, align 8
  store {} %0, ptr %f, align 1
  ret { i8, [16 x i8] } { i8 1, [16 x i8] undef }
}

define internal void @fmt_println({ i64, ptr } %0) {
entry:
  %s = alloca { i64, ptr }, align 8
  store { i64, ptr } %0, ptr %s, align 8
  ret void
}

define internal { i8, [24 x i8] } @read_file({ i64, ptr } %0) {
entry:
  %tmp19 = alloca [24 x i8], align 1
  %tmp12 = alloca [24 x i8], align 1
  %tmp = alloca [24 x i8], align 1
  %path = alloca { i64, ptr }, align 8
  %f = alloca {}, align 8
  %"<match target>" = alloca { i8, [0 x i8] }, align 8
  %f1 = alloca {}, align 8
  %"<wrap>" = alloca { i8, [0 x i8] }, align 8
  %s = alloca { i64, ptr }, align 8
  %"<match target>2" = alloca { i8, [16 x i8] }, align 8
  %s3 = alloca { i64, ptr }, align 8
  %"<wrap>4" = alloca { i8, [16 x i8] }, align 8
  %"<defer>" = alloca { i8, [24 x i8] }, align 8
  store { i64, ptr } %0, ptr %path, align 8
  %load = load { i64, ptr }, ptr %path, align 8
  %os_open = call { i8, [0 x i8] } @os_open({ i64, ptr } %load)
  store { i8, [0 x i8] } %os_open, ptr %"<match target>", align 1
  %ext_tag = extractvalue { i8, [0 x i8] } %os_open, 0
  switch i8 %ext_tag, label %Block_5 [
    i8 0, label %Block_3
  ]

Block_2:                                          ; preds = %Block_4
  %load5 = load {}, ptr %f, align 1
  %io_read_to_string = call { i8, [16 x i8] } @io_read_to_string({} %load5)
  store { i8, [16 x i8] } %io_read_to_string, ptr %"<match target>2", align 1
  %ext_tag6 = extractvalue { i8, [16 x i8] } %io_read_to_string, 0
  switch i8 %ext_tag6, label %Block_12 [
    i8 0, label %Block_10
  ]

Block_3:                                          ; preds = %entry
  %ext_pay = getelementptr inbounds nuw { i8, [0 x i8] }, ptr %"<match target>", i32 0, i32 1
  %load7 = load {}, ptr %ext_pay, align 1
  store {} %load7, ptr %f1, align 1
  %load8 = load {}, ptr %f1, align 1
  store {} %load8, ptr %f, align 1
  br label %Block_4

Block_4:                                          ; preds = %Block_3
  br label %Block_2

Block_5:                                          ; preds = %entry
  store { i8, [0 x i8] } %os_open, ptr %"<wrap>", align 1
  %load9 = load { i8, [0 x i8] }, ptr %"<wrap>", align 1
  store { i8, [0 x i8] } %load9, ptr %tmp, align 1
  %load10 = load [24 x i8], ptr %tmp, align 1
  %set_pay = insertvalue { i8, [24 x i8] } { i8 1, [24 x i8] undef }, [24 x i8] %load10, 1
  store { i8, [24 x i8] } %set_pay, ptr %"<defer>", align 1
  br label %Block_16

Block_9:                                          ; preds = %Block_11
  %load11 = load { i64, ptr }, ptr %s, align 8
  store { i64, ptr } %load11, ptr %tmp12, align 8
  %load13 = load [24 x i8], ptr %tmp12, align 1
  %set_pay14 = insertvalue { i8, [24 x i8] } { i8 0, [24 x i8] undef }, [24 x i8] %load13, 1
  store { i8, [24 x i8] } %set_pay14, ptr %"<defer>", align 1
  br label %Block_16

Block_10:                                         ; preds = %Block_2
  %ext_pay15 = getelementptr inbounds nuw { i8, [16 x i8] }, ptr %"<match target>2", i32 0, i32 1
  %load16 = load { i64, ptr }, ptr %ext_pay15, align 8
  store { i64, ptr } %load16, ptr %s3, align 8
  %load17 = load { i64, ptr }, ptr %s3, align 8
  store { i64, ptr } %load17, ptr %s, align 8
  br label %Block_11

Block_11:                                         ; preds = %Block_10
  br label %Block_9

Block_12:                                         ; preds = %Block_2
  store { i8, [16 x i8] } %io_read_to_string, ptr %"<wrap>4", align 1
  %load18 = load { i8, [16 x i8] }, ptr %"<wrap>4", align 1
  store { i8, [16 x i8] } %load18, ptr %tmp19, align 1
  %load20 = load [24 x i8], ptr %tmp19, align 1
  %set_pay21 = insertvalue { i8, [24 x i8] } { i8 2, [24 x i8] undef }, [24 x i8] %load20, 1
  store { i8, [24 x i8] } %set_pay21, ptr %"<defer>", align 1
  br label %Block_16

Block_16:                                         ; preds = %Block_12, %Block_9, %Block_5
  %load22 = load {}, ptr %f, align 1
  call void @os_close({} %load22)
  br label %Block_17

Block_17:                                         ; preds = %Block_16
  br label %Block_23

Block_23:                                         ; preds = %Block_17
  %load23 = load { i8, [24 x i8] }, ptr %"<defer>", align 1
  ret { i8, [24 x i8] } %load23
}

define internal void @main() {
entry:
  %content = alloca { i64, ptr }, align 8
  %"<match target>" = alloca { i8, [24 x i8] }, align 8
  %content1 = alloca { i64, ptr }, align 8
  %e = alloca { i8, [24 x i8] }, align 8
  %content2 = alloca { i64, ptr }, align 8
  %"<match target>3" = alloca { i8, [24 x i8] }, align 8
  %content4 = alloca { i64, ptr }, align 8
  %panic = alloca { i8, [24 x i8] }, align 8
  %read_file = call { i8, [24 x i8] } @read_file({ i64, ptr } { i64 10, ptr @const_str })
  store { i8, [24 x i8] } %read_file, ptr %"<match target>", align 1
  %ext_tag = extractvalue { i8, [24 x i8] } %read_file, 0
  switch i8 %ext_tag, label %Block_5 [
    i8 0, label %Block_3
  ]

Block_2:                                          ; preds = %Block_4
  %load = load { i64, ptr }, ptr %content, align 8
  call void @fmt_println({ i64, ptr } %load)
  %read_file5 = call { i8, [24 x i8] } @read_file({ i64, ptr } { i64 16, ptr @const_str.1 })
  store { i8, [24 x i8] } %read_file5, ptr %"<match target>3", align 1
  %ext_tag6 = extractvalue { i8, [24 x i8] } %read_file5, 0
  switch i8 %ext_tag6, label %Block_10 [
    i8 0, label %Block_8
  ]

Block_3:                                          ; preds = %entry
  %ext_pay = getelementptr inbounds nuw { i8, [24 x i8] }, ptr %"<match target>", i32 0, i32 1
  %load7 = load { i64, ptr }, ptr %ext_pay, align 8
  store { i64, ptr } %load7, ptr %content1, align 8
  %load8 = load { i64, ptr }, ptr %content1, align 8
  store { i64, ptr } %load8, ptr %content, align 8
  br label %Block_4

Block_4:                                          ; preds = %Block_3
  br label %Block_2

Block_5:                                          ; preds = %entry
  store { i8, [24 x i8] } %read_file, ptr %e, align 1
  call void @fmt_println({ i64, ptr } { i64 11, ptr @const_str.2 })
  ret void

Block_7:                                          ; preds = %Block_9
  %load9 = load { i64, ptr }, ptr %content2, align 8
  call void @fmt_println({ i64, ptr } %load9)
  ret void

Block_8:                                          ; preds = %Block_2
  %ext_pay10 = getelementptr inbounds nuw { i8, [24 x i8] }, ptr %"<match target>3", i32 0, i32 1
  %load11 = load { i64, ptr }, ptr %ext_pay10, align 8
  store { i64, ptr } %load11, ptr %content4, align 8
  %load12 = load { i64, ptr }, ptr %content4, align 8
  store { i64, ptr } %load12, ptr %content2, align 8
  br label %Block_9

Block_9:                                          ; preds = %Block_8
  br label %Block_7

Block_10:                                         ; preds = %Block_2
  store { i8, [24 x i8] } %read_file5, ptr %panic, align 1
  call void @panic()
  unreachable
}
