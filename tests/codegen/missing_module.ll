; ModuleID = 'missing_module'
source_filename = "missing_module"

@panic_msg = private unnamed_addr constant [24 x i8] c"Panic reached! exiting!\00", align 1
@const_str = private constant [21 x i8] c"this will have errors"

define void @"<init>"() {
entry:
  ret void
}

define void @fmt_println({ i64, ptr } %0) {
entry:
  %s = alloca { i64, ptr }, align 8
  store { i64, ptr } %0, ptr %s, align 8
  ret void
}

define void @main() {
entry:
  call void @fmt_println({ i64, ptr } { i64 21, ptr @const_str })
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
