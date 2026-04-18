; ModuleID = 'simple_add'
source_filename = "simple_add"

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
  %b = alloca i64, align 8
  %c = alloca i64, align 8
  %d = alloca i64, align 8
  store i64 10, ptr %a, align 8
  store i64 20, ptr %b, align 8
  store i64 5, ptr %c, align 8
  store i64 -9223372036854775617, ptr %d, align 8
  %load = load i64, ptr %a, align 8
  %load1 = load i64, ptr %b, align 8
  %iadd = add i64 %load, %load1
  %load2 = load i64, ptr %c, align 8
  %isub = sub i64 %iadd, %load2
  ret i64 %isub
}
