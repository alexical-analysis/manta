; ModuleID = 'loops'
source_filename = "loops"

@panic_msg = private unnamed_addr constant [24 x i8] c"Panic reached! exiting!\00", align 1

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

define void @"<init>"() {
entry:
  ret void
}

define i64 @main() {
entry:
  %a = alloca i64, align 8
  store i64 0, ptr %a, align 8
  br label %Block_2

Block_2:                                          ; preds = %Block_6, %entry
  %load = load i64, ptr %a, align 8
  %iadd = add i64 %load, 1
  store i64 %iadd, ptr %a, align 8
  %load1 = load i64, ptr %a, align 8
  %sgt = icmp sgt i64 %load1, 255
  br i1 %sgt, label %Block_3, label %Block_4

Block_3:                                          ; preds = %Block_2
  br label %Block_7

Block_4:                                          ; preds = %Block_2
  br label %Block_6

Block_6:                                          ; preds = %Block_4
  br label %Block_2

Block_7:                                          ; preds = %Block_3
  %load2 = load i64, ptr %a, align 8
  ret i64 %load2
}
