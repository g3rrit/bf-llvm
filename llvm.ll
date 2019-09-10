
@MEM = global [65536 x i8] zeroinitializer
@MEM_POS = global i32 32768

%FILE = type opaque
declare i32 @fflush(%FILE*)


declare i32 @putchar(i8)
declare i32 @getchar()

define i32 @main() {

  ; > <
  %1 = load i32, i32* @MEM_POS
  %2 = add i32 %1, 1 ; sub i32 %i, 1
  store i32 %2, i32* @MEM_POS

  ; + -
  %3 = load i32, i32* @MEM_POS
  %4 = getelementptr [65536 x i8], [65536 x i8]* @MEM, i32 0, i32 %3
  %5 = load i8, i8* %4
  %6 = add i8 %5, 1 ; sub i8 %5, 1
  store i8 %6, i8* %4

  ; .
  %7 = load i32, i32* @MEM_POS
  %8 = getelementptr [65536 x i8], [65536 x i8]* @MEM, i32 0, i32 %7
  %9 = load i8, i8* %8
  call i32 @putchar(i8 %9)

  ; ,
  %11 = call i32 @getchar()
  %12 = trunc i32 %11 to i8
  %13 = load i32, i32* @MEM_POS
  %14 = getelementptr [65536 x i8], [65536 x i8]* @MEM, i32 0, i32 %13
  store i8 %12, i8* %14

  ; .
  %15 = load i32, i32* @MEM_POS
  %16 = getelementptr [65536 x i8], [65536 x i8]* @MEM, i32 0, i32 %15
  %17 = load i8, i8* %16
  call i32 @putchar(i8 %17)
  call i32 @fflush(%FILE* null)

  ; [
  %20 = load i32, i32* @MEM_POS
  %21 = getelementptr [65536 x i8], [65536 x i8]* @MEM, i32 0, i32 %20
  %22 = load i8, i8* %21
  %23 = icmp eq i8 %22, 0
  br i1 %23, label %r.0, label %l.0
l.0:

  ; ]
  %24 = load i32, i32* @MEM_POS
  %25 = getelementptr [65536 x i8], [65536 x i8]* @MEM, i32 0, i32 %24
  %26 = load i8, i8* %25
  %27 = icmp eq i8 %26, 0
  br i1 %27, label %r.0, label %l.0
r.0:

  ret i32 0
}
