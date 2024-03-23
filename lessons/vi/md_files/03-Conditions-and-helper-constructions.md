# Câu lệnh điều kiện và các cấu trúc bổ trợ

## Nội dung

- Biểu thức if-then-else.

- Guards

- Biểu thức `let`

- `where`

- Nên sử dụng `let` hay `where` ?

- Những điều cần lưu ý

## Biểu thức if-then-else

Trong code, bạn thường phải đưa ra một số lựa chọn rẽ nhánh. Có một số cách để biểu thị điều kiện rẽ nhánh. Trong Haskell, chúng ta thường sử dụng biểu thức **if-then-else** :

```haskell
if <Condition>
  then <Expesssion1>
  else <Expesssion2>
```

Trong đó `Condition` là biểu thức logic trả về kết quả `True` hoặc `False` , `Expression1` là biểu thức được thực hiện nếu `Condition` là `True` và `Expression2` là biểu thức được thực hiện nếu `Condition` là `False` . Hàm `checkLocalHost` dưới đây kiểm tra xem đối số có phải là localhost hay không và thông báo cho người dùng.

```haskell
checkLocalhost :: String -> String
checkLocalhost ip =
    -- True or False?
    if ip == "127.0.0.1"
        -- When the condition is True the answer is
        then "It's localhost!"
        -- Otherwise the condition is False and the answer is
        else "No, it's not localhost."

checkLocalhost "127.0.0.1"
```

```
"It's localhost!"
```

Hàm `checkLocalhost` được áp dụng cho một đối số duy nhất thuộc kiểu `String` và trả về một giá trị khác thuộc kiểu `String`. Đối số là một chuỗi `ip` chứa địa chỉ IP và hàm sẽ kiểm tra xem chuỗi đó có bằng `"127.0.0.1"` hay không. Nếu kiểm tra thành công, hàm sẽ trả về `"It's localhost!"` , nếu không nó sẽ trả về `"No, it's not localhost."`

<div class="alert alert-block alert-info">Trong khi với các ngôn ngữ lập trình mệnh lệnh, <code>else</code> không bắt buộc, nhưng trong Haskell thì ngược lại! Đó là vì trong Haskell, mọi hàm đều phải trả về một giá trị. Vì vậy, chúng ta có nghĩa vụ cung cấp kết quả cùng kiểu cho cả trường hợp <code>then</code> và <code>else</code> .</div>

## Guards

Bây giờ, hãy tưởng tượng rằng chúng ta muốn thực hiện một phép kiểm tra phức tạp hơn. Giống như kiểm tra xem sinh nhật năm nay có ý nghĩa gì đặc biệt không. Chúng ta có thể sử dụng các câu lệnh if-else lồng nhau như sau:

```haskell
specialBirthday :: Int -> [Char]
specialBirthday age =
  if age == 1
    then "First birthday!"
    else
      if age == 18
        then "You're an adult!"
        else
          if age == 60
            then "Finally, I can stop caring about new lingo!"
            else "Nothing special"
```

&lt;style&gt;/* Styles used for the Hoogle display in the pager */ .hoogle-doc { display: block; padding-bottom: 1.3em; padding-left: 0.4em; } .hoogle-code { display: block; font-family: monospace; white-space: pre; } .hoogle-text { display: block; } .hoogle-name { color: green; font-weight: bold; } .hoogle-head { font-weight: bold; } .hoogle-sub { display: block; margin-left: 0.4em; } .hoogle-package { font-weight: bold; font-style: italic; } .hoogle-module { font-weight: bold; } .hoogle-class { font-weight: bold; } .get-type { color: green; font-weight: bold; font-family: monospace; display: block; white-space: pre-wrap; } .show-type { color: green; font-weight: bold; font-family: monospace; margin-left: 1em; } .mono { font-family: monospace; display: block; } .err-msg { color: red; font-style: italic; font-family: monospace; white-space: pre; display: block; } #unshowable { color: red; font-weight: bold; } .err-msg.in.collapse { padding-top: 0.7em; } .highlight-code { white-space: pre; font-family: monospace; } .suggestion-warning { font-weight: bold; color: rgb(200, 130, 0); } .suggestion-error { font-weight: bold; color: red; } .suggestion-name { font-weight: bold; } &lt;/style&gt;<div class="suggestion-name" style="clear:both;">Use guards</div>
<div class="suggestion-row" style="float: left;">
<div class="suggestion-warning">Found:</div>
<div class="highlight-code" id="haskell">specialBirthday age </div> <p data-md-type="paragraph">= if age == 1 then "First birthday!" else if age == 18 then "You're an adult!" else if age == 60 then "Finally, I can stop caring about new lingo!" else "Nothing special"</p>
</div>


<div class="suggestion-row" style="float: left;">
<div class="suggestion-warning">Tại sao không phải là:</div>
<div class="highlight-code" id="haskell">specialBirthday age | age == 1 = "First birthday!" | age == 18 = "You're an adult!" | age == 60 = "Finally, I can stop caring about new lingo!" | otherwise = "Nothing special"</div>
</div>

Đó là một mớ hỗn độn! Quá phức tạp để đọc và viết. May mắn thay, chúng ta có <strong>guards</strong>!

Guards hoạt động tương tự như câu lệnh if-else, nhưng bạn có thể có nhiều điều kiện:

```haskell
func arg
  | <Condition1> = <Result1>
  | <Condition2> = <Result2>
  | <Condition3> = <Result3>
  ...
```

Chúng ta sử dụng ký hiệu `|` để bắt đầu mỗi guard.

<div class="alert alert-block alert-info">     Lưu ý rằng không có dấu <code>=</code> sau các đối số của <code>func</code> ! Đó là một lỗi phổ biến khi viết Guards. Đừng thêm <code>=</code> !</div>

Với Guards, chúng ta có thể viết hàm `specialBirthday` như thế này:

```haskell
specialBirthday :: Int -> [Char]
specialBirthday age
  | age == 1 = "First birthday!"
  | age == 18 = "You're an adult!"
  | age == 60 = "Finally, I can stop caring about new lingo!"
  | True = "Nothing special"
```

&lt;style&gt;/* Styles used for the Hoogle display in the pager */ .hoogle-doc { display: block; padding-bottom: 1.3em; padding-left: 0.4em; } .hoogle-code { display: block; font-family: monospace; white-space: pre; } .hoogle-text { display: block; } .hoogle-name { color: green; font-weight: bold; } .hoogle-head { font-weight: bold; } .hoogle-sub { display: block; margin-left: 0.4em; } .hoogle-package { font-weight: bold; font-style: italic; } .hoogle-module { font-weight: bold; } .hoogle-class { font-weight: bold; } .get-type { color: green; font-weight: bold; font-family: monospace; display: block; white-space: pre-wrap; } .show-type { color: green; font-weight: bold; font-family: monospace; margin-left: 1em; } .mono { font-family: monospace; display: block; } .err-msg { color: red; font-style: italic; font-family: monospace; white-space: pre; display: block; } #unshowable { color: red; font-weight: bold; } .err-msg.in.collapse { padding-top: 0.7em; } .highlight-code { white-space: pre; font-family: monospace; } .suggestion-warning { font-weight: bold; color: rgb(200, 130, 0); } .suggestion-error { font-weight: bold; color: red; } .suggestion-name { font-weight: bold; } &lt;/style&gt;<div class="suggestion-name" style="clear:both;">Use otherwise</div>
<div class="suggestion-row" style="float: left;">
<div class="suggestion-warning">Found:</div>
<div class="highlight-code" id="haskell">specialBirthday age </div> <p data-md-type="paragraph">| age == 1 = "First birthday!" | age == 18 = "You're an adult!" | age == 60 = "Finally, I can stop caring about new lingo!" | True = "Nothing special"</p>
</div>


<div class="suggestion-row" style="float: left;">
<div class="suggestion-warning">Tại sao không phải là:</div>
<div class="highlight-code" id="haskell">specialBirthday age | age == 1 = "First birthday!" | age == 18 = "You're an adult!" | age == 60 = "Finally, I can stop caring about new lingo!" | otherwise = "Nothing special"</div>
</div>

Nhánh `True` cuối cùng đóng vai trò là một điều kiện "bắt tất cả". Một điều kiện luôn đúng vì nó luôn bằng `True` .

Việc có điều kiện `True` trong guard cuối cùng phổ biến đến mức Haskell cung cấp sẵn một biến được gọi là `otherwise` luôn bằng `True` ( `otherwise = True` ) giúp guard dễ đọc hơn:

```haskell
specialBirthday :: Int -> [Char]
specialBirthday age
  | age == 1 = "First birthday!"
  | age == 18 = "You're an adult!"
  | age == 60 = "Finally, I can stop caring about new lingo!"
  | otherwise = "Nothing special"

specialBirthday 60
```

```
"Finally, I can stop caring about new lingo!"
```

Bây giờ bạn có thể dễ dàng hiểu được biểu thức này làm gì chỉ bằng một cái nhìn thoáng qua!

Được rồi, vậy là xong về các phép kiểm tra điều kiện. Bây giờ, hãy xem làm thế nào để nâng tầm cách chúng ta viết hàm với `let` và `where` !

## `let` và `where`

Chúng ta sử dụng `let` và `where` để lưu trữ kết quả tính toán trung gian và gán giá trị cho các biến cục bộ.

Hãy bắt đầu với `let` !

### Biểu thức `let`

`let` có thể gán các biểu thức cho các biến cục bộ theo cách sau:

```haskell
func arg =
    let <BIND_1>
        <BIND_2>
    in  <EXPR that uses BIND_1 and/or BIND_2>
```

Trong đó `<BIND_X>` là các phép gán cục bộ có thể truy cập được trong toàn bộ biểu thức `let` .

Bây giờ, hãy tạo một hàm nhận hai nhiệt độ—một ở độ C và một ở độ F—và trả về nhiệt độ nóng hơn nhưng ở độ Kelvin. Có khá nhiều phép quy đổi, phải không?

Để chuyển từ độ F sang độ C, trước tiên chúng ta phải trừ 32 rồi nhân với 5/9, như sau:

<code>tC = (tF - 32) * 5/9</code>

Để chuyển từ độ C sang độ Kelvin, chúng ta chỉ cần cộng 273,16 như thế này:

<code>tK = tC + 273.16</code>

Vì vậy, nếu muốn tạo **một hàm duy nhất** thực hiện tất cả những điều đó, chúng ta có thể làm như sau:

```haskell
hotterInKelvin :: Double -> Double -> Double
hotterInKelvin c f = if c > (f - 32) * 5 / 9 then c + 273.16 else ((f - 32) * 5 / 9) + 273.16

hotterInKelvin 40 100
```

```
313.16
```

Nó hoạt động, nhưng code khá khó đọc.

Một cách tiếp cận tốt hơn là sử dụng các phép gán `let` cho các biểu thức trung gian và viết biểu thức cuối cùng trong phần `in` kết nối mọi thứ lại với nhau:

```haskell
hotterInKelvin' :: Double -> Double -> Double
hotterInKelvin' c f =
  let fToC t = (t - 32) * 5 / 9
      cToK t = t + 273.16
      fToK t = cToK (fToC t)
   in if c > fToC f then cToK c else fToK f

hotterInKelvin' 40 100
```

```
313.16
```

Bây giờ code của chúng ta dễ đọc hơn nhiều và không có các biểu thức bị lặp lại!

Nhưng xin chờ chút nữa! Chúng ta cũng có thể sử dụng cấu trúc `where` !

### `where`

Chúng ta có thể sử dụng `where` để gán các giá trị cho các biến theo cách sau:

```haskell
func arg = <EXP that uses BIND_1 and/or BIND_2>
    where <BIND_1>
          <BIND_2>
```

Vì vậy, hàm `hotterInKelvin` tương tự như trên có thể được viết với `where` như thế này:

Trong đó `<BIND_X>` là các phép gán có thể truy cập được trong toàn bộ nội dung hàm.

```haskell
hotterInKelvin'' :: Double -> Double -> Double
hotterInKelvin'' c f = if c > fToC f then cToK c else fToK f
  where
    fToC t = (t - 32) * 5 / 9
    cToK t = t + 273.16
    fToK t = cToK (fToC t)

hotterInKelvin'' 40 100
```

```
313.16
```

Ồ, cả hai dường như làm cùng một việc. Vậy, tại sao lại phải có cả hai? Liệu chúng ta có thể chỉ chọn sử dụng một trong số chúng không?

Vâng, có rất nhiều trường hợp mà chúng có thể thay thế được cho nhau. Trong những trường hợp đó, bạn có thể chọn bất kỳ cái nào bạn thích nhất. Nhưng chúng cũng có những hạn chế và ưu điểm riêng.

### Nên sử dụng `let` hay `where` ?

Biểu thức `let` thuận tiện bất cứ khi nào chúng ta muốn chia các biểu thức phức tạp thành các khối nhỏ hơn rồi kết hợp chúng lại trong biểu thức cuối cùng.

Ví dụ, hãy tưởng tượng bạn muốn tính thể tích của một ngôi nhà. Chúng ta có thể đơn giản hóa vấn đề như sau:

Ngôi nhà là một khối lập phương có hình chóp ở trên (mái nhà). Vì vậy, để tìm thể tích của nó, chúng ta cần tính thể tích của hình lập phương và thể tích của hình chóp rồi cộng chúng lại với nhau:

```haskell
houseV side roofH = let cubeV = side ^ 3
                        pyramidV = (side ^ 2) * roofH / 3
                    in  cubeV + pyramidV
                    
houseV 3 1
```

```
30.0
```

Chúng ta tạo các phép tính trung gian `cubeV` và `pyramidV` bên trong khối `let` và sau đó sử dụng chúng bên trong biểu thức `in`.

Bên cạnh sự rõ ràng của cú pháp, một ưu điểm khác là nếu biểu thức cuối cùng sau này trở nên phức tạp hơn (ví dụ: thêm một ống khói vào ngôi nhà), chúng ta chỉ cần thêm một phép gán khác và sử dụng nó trong biểu thức cuối cùng!:

```haskell
houseV side roofH = let cubeV = side ^ 3
                        pyramidV = (side ^ 2) * roofH / 3
                        chimneyV = (0.5 ^ 2) * roofH
                    in  cubeV + pyramidV + chimneyV
                    
houseV 3 1
```

```
30.25
```

Ở chiều hướng khác, biểu thức `where` thuận tiện khi chúng ta cần các phép gán có thể sử dụng được trên các nhánh guard.

Bởi vì chúng ta không thể truy cập các phép gán `let` trên tất cả các Guard, nhưng với  `where` thì chúng ta có thể!! Ví dụ:

```haskell
analyzeCylinder :: Float -> Float -> String
analyzeCylinder diameter height
       | volume < 10 = "The cylinder is a glass."
       | volume < 100 = "The cylinder is a bucket."
       | volume < 1000 = "The cylinder is a tank."
       | otherwise = "What in the world is that huge thing?!"
    where
        volume = pi * diameter^2 * height / 4

analyzeCylinder 15 6
```

```
"What in the world is that huge thing?!"
```

Như bạn có thể thấy, chúng ta có phép gán biến `volume` bên trong khối `where` và sau đó chúng ta truy cập nó trên mọi biểu thức guard!

Và cuối cùng, sự khác biệt chính giữa hai loại này là với `where` các khai báo được giới hạn trong một cấu trúc cú pháp xung quanh. Nghĩa là chúng chỉ có thể được sử dụng trong một phạm vi cụ thể (ví dụ bên trong thân hàm). Còn `let` là một biểu thức, nên nó có thể được sử dụng ở bất cứ nơi nào mà một biểu thức có thể được sử dụng. Ví dụ:

```haskell
-- Seconds in a day
24 * (let seconds = 60 in seconds * 60)

-- The volume of a rectangular prism (we can separate expressions by semicolons to have them in the same line)
let s1 = 10; s2 = 20; s3 = 30; in s1*s2*s3
```

```
86400



6000
```

Trong tất cả các trường hợp khi bạn có thể sử dụng cả hai, hãy chọn cái phù hợp với tình huống hoặc phong cách của bạn. Cần một chút thực hành để lựa chọn cái nào phù hợp và tùy thuộc vào sở thích của lập trình viên. Vì vậy, đừng nghĩ ngợi quá nhiều về nó.

### Những điều cần lưu ý

Các biểu thức được định nghĩa bởi `where` không thể truy cập được bên ngoài thân hàm đó.

```haskell
fToK t = 273.16 + fToC t
    where fToC t = (t - 32) * 5 / 9
    
fToC 60
```

```
<interactive>:1:1: error:
    • Variable not in scope: fToC :: t0 -> t
    • Perhaps you meant ‘fToK’ (line 1)
```

Biểu thức được khai báo trong biểu thức `let` chỉ tồn tại trong biểu thức `let` đó.

Ví dụ: hàm này lấy tên và họ của bạn rồi trả về tên viết tắt của bạn:

```haskell
initials :: String -> String -> String
initials name lastName = if name == "" || lastName == ""
                         then "How was your name again?"
                         else let x = head name
                                  y = head lastName
                              in [x] ++ "." ++ [y] ++ "."

initials "Richard" "Feynman"
```

```
"R.F."
```

Các biểu thức `x` và `y` chỉ khả dụng bên trong biểu thức `let` đó. Nếu bạn cố gắng sử dụng chúng bên trong `if` hoặc `then`, thì chúng nằm ngoài phạm vi sử dụng và sẽ không biên dịch được.

## Tổng kết

Trong bài học này, chúng ta đã thảo luận về:

- Câu lệnh if-then-else và lý do tại sao bạn luôn phải định nghĩa trường hợp else.

- Cách sử dụng guards để tránh các câu lệnh if-else lồng nhau.

- Cách sử dụng `let` và `where` lưu trữ kết quả của các phép tính trung gian, liên kết các biến cục bộ, cho phép mã sạch hơn và tránh lặp code.
