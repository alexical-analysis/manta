; ModuleID = 'hello'
source_filename = "hello"

@hello_str = private unnamed_addr constant [13 x i8] c"Hello World!\00", align 1

declare i32 @puts(ptr)

define i32 @main() {
entry:
  %call_puts = call i32 @puts(ptr @hello_str)
  ret i32 %call_puts
}
