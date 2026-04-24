; ModuleID = 'structs'
source_filename = "structs"

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

define void @"manta_<init>"() {
entry:
  ret void
}

define internal i32 @two() {
entry:
  ret i32 2
}

define internal i32 @main() {
entry:
  %a = alloca { i32, double, i64 }, align 8
  %b = alloca { i8, i32 }, align 8
  %c = alloca { i32, double, i64 }, align 8
  store { i32, double, i64 } { i32 10, double 2.000000e+01, i64 30 }, ptr %a, align 8
  %struct_gep = getelementptr inbounds nuw { i32, double, i64 }, ptr %a, i32 0, i32 0
  %load = load i32, ptr %struct_gep, align 4
  %iadd = add i32 %load, 5
  %set_field = insertvalue { i8, i32 } { i8 -128, i32 undef }, i32 %iadd, 1
  store { i8, i32 } %set_field, ptr %b, align 4
  %struct_gep1 = getelementptr inbounds nuw { i8, i32 }, ptr %b, i32 0, i32 1
  %load2 = load i32, ptr %struct_gep1, align 4
  %sdiv = sdiv i32 %load2, 2
  %two = call i32 @two()
  %iadd3 = add i32 %sdiv, %two
  %struct_gep4 = getelementptr inbounds nuw { i32, double, i64 }, ptr %a, i32 0, i32 2
  %load5 = load i64, ptr %struct_gep4, align 8
  %iadd6 = add i64 %load5, 1000
  %set_field7 = insertvalue { i32, double, i64 } undef, i32 %iadd3, 0
  %set_field8 = insertvalue { i32, double, i64 } %set_field7, double 3.141500e+00, 1
  %set_field9 = insertvalue { i32, double, i64 } %set_field8, i64 %iadd6, 2
  store { i32, double, i64 } %set_field9, ptr %c, align 8
  %struct_gep10 = getelementptr inbounds nuw { i32, double, i64 }, ptr %c, i32 0, i32 0
  %load11 = load i32, ptr %struct_gep10, align 4
  ret i32 %load11
}
