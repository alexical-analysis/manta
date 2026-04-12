; ModuleID = 'pointers'
source_filename = "pointers"

@panic_msg = private unnamed_addr constant [24 x i8] c"Panic reached! exiting!\00", align 1

define void @"<init>"() {
entry:
  ret void
}

define void @fmt_println(i64 %0) {
entry:
  %i = alloca i64, align 8
  store i64 %0, ptr %i, align 4
  ret void
}

define void @main() {
entry:
  %i = alloca i64, align 8
  %j = alloca i64, align 8
  %p = alloca ptr, align 8
  store i64 42, ptr %i, align 8
  store i64 2701, ptr %j, align 8
  store ptr %i, ptr %p, align 8
  %load = load ptr, ptr %p, align 8
  %load1 = load i64, ptr %load, align 8
  call void @fmt_println(i64 %load1)
  %load2 = load ptr, ptr %p, align 8
  store i64 21, ptr %load2, align 8
  %load3 = load i64, ptr %i, align 8
  call void @fmt_println(i64 %load3)
  store ptr %j, ptr %p, align 8
  %load4 = load ptr, ptr %p, align 8
  %load5 = load i64, ptr %load4, align 8
  %sdiv = sdiv i64 %load5, 37
  %load6 = load ptr, ptr %p, align 8
  store i64 %sdiv, ptr %load6, align 8
  %load7 = load i64, ptr %j, align 8
  call void @fmt_println(i64 %load7)
  ret void
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
