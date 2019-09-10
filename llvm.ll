
@MEM = global [65536 x i32] zeroinitializer
@MEM_POS = global i32 32768

%struct._IO_FILE = type opaque
declare i32 @fflush(%struct._IO_FILE*)


declare i32 @putchar(i8)
declare i32 @getchar()

define i32 @main() {

  ; > <
  %1 = load i32, i32* @MEM_POS
  %2 = add i32 %1, 1 ; sub i32 %i, 1
  store i32 %2, i32* @MEM_POS

  ; + -
  %3 = load i32, i32* @MEM_POS
  %4 = getelementptr [65536 x i32], [65536 x i32]* @MEM, i32 0, i32 %3
  %5 = load i32, i32* %4
  %6 = add i32 %5, 1 ; sub i32 %5, 1
  store i32 %6, i32* %4

  ; .
  %7 = load i32, i32* @MEM_POS
  %8 = getelementptr [65536 x i32], [65536 x i32]* @MEM, i32 0, i32 %7
  %9 = load i32, i32* %8
  %10 = trunc i32 %9 to i8
  call i32 @putchar(i8 %10)

  ; ,
  %12 = call i32 @getchar()
  %13 = load i32, i32* @MEM_POS
  %14 = getelementptr [65536 x i32], [65536 x i32]* @MEM, i32 0, i32 %13
  store i32 %12, i32* %14

  ; .
  %15 = load i32, i32* @MEM_POS
  %16 = getelementptr [65536 x i32], [65536 x i32]* @MEM, i32 0, i32 %15
  %17 = load i32, i32* %16
  %18 = trunc i32 %17 to i8
  call i32 @putchar(i8 %18)
  call i32 @fflush(%struct._IO_FILE* null)

  ; [
  %21 = load i32, i32* @MEM_POS
  %22 = icmp eq i32 %21, 0
  br i1 %22, label %r.0, label %l.0
l.0:

  ; ]
  %23 = load i32, i32* @MEM_POS
  %24 = icmp eq i32 %23, 0
  br i1 %24, label %r.0, label %l.0
r.0:

  ret i32 0
}
