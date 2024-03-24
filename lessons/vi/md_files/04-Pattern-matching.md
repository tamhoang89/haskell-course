# Khớp mẫu và biểu thức Case

## Nội dung

- Khớp mẫu trong hàm
    - Mẫu "bắt tất cả"
- Xem xét kỹ hơn các danh sách
- Khớp mẫu
    - Danh sách
    - Bộ dữ liệu
- Biểu thức Case
- Phong cách khai báo vs Phong cách biểu thức

## Khớp mẫu - Pattern matching

**Khớp mẫu** là hành động so khớp dữ liệu (giá trị, loại, v.v.) với một mẫu, gán các biến tùy ý với các kết quả khớp thành công.

Chúng ta sẽ thảo luận về việc khớp mẫu trong ba trường hợp:

- Khớp mẫu trong định nghĩa hàm.

- Khớp mẫu cho danh sách.

- Khớp mẫu cho các tuple.

Nghe có vẻ phức tạp nhưng nó thực sự khá trực quan khi bạn hiểu rõ về nó. Mọi chuyện sẽ rõ ràng sau vài ví dụ.

Hãy dùng khớp mẫu với một số hàm!

## Khớp mẫu trong hàm

Bạn có nhớ hàm `specialBirthday` của bài học trước không?

```haskell
specialBirthday :: Int -> [Char]
specialbirthday age =
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

Tôi biết ... Chúng ta đã khắc phục sự rườm rà đó với guards. Nhưng bây giờ, chúng ta sẽ sáng tạo hơn và giải quyết vấn đề bằng khớp mẫu (Pattern matching)!

Để khớp mẫu trên định nghĩa hàm, chúng ta chỉ cần định nghĩa hàm đó nhiều lần, thay thế các tham số bằng các giá trị. Như thế này:

```haskell
specialBirthday :: Int -> [Char]
specialBirthday 1   = "First birthday!"
specialBirthday 18  = "You're an adult!"
specialBirthday 60  = "finally, I can stop caring about new lingo!"
```

Hàm của chúng ta đã được định nghĩa! Và nó trông đẹp hơn trước rất nhiều!

Và nó hoạt động như thế nào? Chà, khi gặp mã code như thế này, Haskell sẽ thử khớp giá trị của `age` với định nghĩa đầu tiên. Nếu `age /= 1`, nó sẽ thử khớp tiếp với định nghĩa thứ hai. Nếu `age /= 18`, nó sẽ thử tiếp với định nghĩa thứ ba. Và cứ tiếp tục như vậy cho đến khi giá trị tham số khớp với một trong các giá trị của định nghĩa.

Và chắc rằng bạn đã nhận thấy một vấn đề lớn. Điều gì xảy ra nếu chúng ta truyền vào một số khác với số được định nghĩa? Ví dụ 29? Chúng ta có thể giải quyết điều đó bằng mẫu tổng quát - catch-all pattern!

### Mẫu tổng quát (Catch-all patterns)

Chữ ký của hàm nêu rõ rằng bạn có thể truyền bất kỳ giá trị nào thuộc loại `Int` .

Vì vậy, chúng ta có thể truyền bất kỳ số nào cho hàm, ví dụ `14`. Nhưng hàm này sẽ làm gì nếu truyền vào `14` ? Chúng ta không chỉ định nó vì chúng ta không khớp mẫu cho `14` ! Vì vậy, chương trình sẽ bị lỗi 🔥 vì không biết cách xử lý giá trị đó! 😱

Bởi vì chúng ta cần hàm hoạt động với bất kỳ giá trị nào mà kiểu dữ liệu của chúng ta chấp nhận, nên chúng ta cần khớp mẫu cho tất cả các tình huống có thể xảy ra. Nhưng bạn không thể viết định nghĩa cho từng giá trị đơn lẻ! Vậy bạn có thể làm gì?!?!

Bạn sử dụng một mẫu tổng quát - catch-all pattern!

**Mẫu tổng quát cho phép bạn cung cấp một định nghĩa mặc định trong trường hợp không có mẫu cụ thể nào của bạn phù hợp.**

Trong trường hợp này, nó sẽ đóng vai trò `else` ở cuối `specialBirthday` .

Để sử dụng mẫu tổng quát, bạn phải cung cấp một tên bắt đầu bằng chữ cái thường, như `age` , `x` hoặc `yearsSinceThisPoorSoulHasTouchedTheEarth` .

Như thế này:

```haskell
specialBirthday :: Int -> [Char]
specialBirthday 1   = "First birthday!"
specialBirthday 18  = "You're an adult!"
specialBirthday 60  = "finally, I can stop caring about new lingo!"
specialBirthday age = "Nothing special"

specialBirthday 18
```

```
"You're an adult!"
```

Bây giờ, nếu chúng ta truyền vào bất kỳ số nào khác `1` , `18` hoặc `60` , `specialBirthday` sẽ trả về `"Nothing special"` .

<div class="alert alert-block alert-warning"> <b>QUAN TRỌNG:</b> Luôn cung cấp các mẫu cho tất cả các tình huống có thể xảy ra! Nếu không, bạn sẽ nhận được cảnh báo dưới đây:</div>
<p data-md-type="paragraph"><code data-md-type="codespan">Pattern match(es) are non-exhaustive In an equation for specialBirthday</code></p>
<div data-md-type="block_html"></div>

Một chi tiết quan trọng khác là Haskell khớp từ trên xuống dưới. Vì vậy, nếu bạn làm điều gì đó như:

```haskell
specialBirthday :: Int -> [Char]
specialBirthday age = "Nothing special"
specialBirthday 1   = "First birthday!"
specialBirthday 18  = "You're an adult!"
specialBirthday 60  = "finally, I can stop caring about new lingo!"

specialBirthday 60
```

```
"Nothing special"
```

Định nghĩa đầu tiên sẽ bắt tất cả các tình huống và kết quả là chúng ta sẽ luôn nhận được `"Nothing special"` , bất kể số truyền vào là gì. Vì vậy, hãy đảm bảo mẫu tổng quát nằm ở vị trí cuối cùng.

Cuối cùng, chúng tôi đã nói rằng bạn có thể tùy ý **gán các biến với các kết quả khớp thành công** và đó là những gì chúng ta vừa làm!

Khi sử dụng `specialBirthday` , mỗi khi giá trị rơi vào mẫu tổng quát `age` , chúng ta gán giá trị đó cho biến `age` . Cho phép chúng ta sử dụng giá trị bên trong biểu thức của định nghĩa (giống như một đối số)!:

```haskell
-- Note: You should know how to use `show` if you did last week homework.

specialBirthday :: Int -> [Char]
specialBirthday 1   = "First birthday!"
specialBirthday 18  = "You're an adult!"
specialBirthday 60  = "finally, I can stop caring about new lingo!"
specialBirthday age = "Nothing special, you're just " ++ show age

specialBirthday 22
```

```
"Nothing special, you're just 22"
```

Bạn không thể phủ nhận sự hữu dụng của điều này! **Bạn đang lọc ra những giá trị phù hợp với một mẫu cụ thể ĐỒNG THỜI gán chúng vào các biến để sử dụng sau này!**

Một ví dụ hấp dẫn hơn về sự hữu ích của tính năng này là khi khớp mẫu với các cấu trúc phức tạp hơn như danh sách và tuple. Hãy cùng khám phá điều đó.

## Xem xét kỹ hơn các danh sách

Trước khi tìm hiểu về khớp mẫu với danh sách, chúng ta cần xem xét kỹ hơn về danh sách.

Chúng ta biết rằng toán tử `:` (cons) thêm một phần tử vào đầu danh sách:

```haskell
-- (:) :: a -> [a] -> [a]

3 : [4,5]  -- [3,4,5]

'L' : "ook, mom! I'm programming"  -- "I'm programming"
```

```
[3,4,5]

"Look, mom! I'm programming"
```

Bạn có nhớ tôi đã nói với bạn rằng `String` là syntactic sugar (cách viết dễ đọc) của `[Char]` không? Chà, một điều bất ngờ nữa là **cách chúng ta viết danh sách cho đến nay thực ra cũng là một syntactic sugar trong Haskell. Cách Haskell thực sự nhìn nhận danh sách là một danh sách trống được thêm vào trước tất cả các phần tử mà nó chứa!** 🤯

```haskell
[1,2,3,4] == 1:2:3:4:[]  -- True

"Hello!"  == 'H':'e':'l':'l':'o':'!':[]  -- True
```

&lt;style&gt;/* Styles used for the Hoogle display in the pager */ .hoogle-doc { display: block; padding-bottom: 1.3em; padding-left: 0.4em; } .hoogle-code { display: block; font-family: monospace; white-space: pre; } .hoogle-text { display: block; } .hoogle-name { color: green; font-weight: bold; } .hoogle-head { font-weight: bold; } .hoogle-sub { display: block; margin-left: 0.4em; } .hoogle-package { font-weight: bold; font-style: italic; } .hoogle-module { font-weight: bold; } .hoogle-class { font-weight: bold; } .get-type { color: green; font-weight: bold; font-family: monospace; display: block; white-space: pre-wrap; } .show-type { color: green; font-weight: bold; font-family: monospace; margin-left: 1em; } .mono { font-family: monospace; display: block; } .err-msg { color: red; font-style: italic; font-family: monospace; white-space: pre; display: block; } #unshowable { color: red; font-weight: bold; } .err-msg.in.collapse { padding-top: 0.7em; } .highlight-code { white-space: pre; font-family: monospace; } .suggestion-warning { font-weight: bold; color: rgb(200, 130, 0); } .suggestion-error { font-weight: bold; color: red; } .suggestion-name { font-weight: bold; } &lt;/style&gt;<br>Use list literal<br>Found:<br>1 : 2 : 3 : 4 : []<br>Why Not:<br>[1, 2, 3, 4]<br>Use list literal<br>Found:<br>'H' : 'e' : 'l' : 'l' : 'o' : '!' : []<br>Why Not:<br>['H', 'e', 'l', 'l', 'o', '!']

```
True
True
```

Bây giờ, bạn có thể nghĩ: "Tại sao tôi phải quan tâm? Tôi sẽ vẫn viết danh sách như mọi khi." Với những gì tôi sẽ nói: "AHA! KHỚP MẪU!!"

## Khớp mẫu với danh sách

Bây giờ chúng ta đã biết danh sách trông như thế nào khi không trang điểm 💅, chúng ta có thể sử dụng nó để khớp mẫu với các định nghĩa hàm khác nhau tùy thuộc vào cấu trúc của danh sách!

Hãy tiến hành khớp mẫu theo các cách khác nhau và cùng khám phá cách hoạt động của đoạn code sau:

```haskell
whatsInsideThisList :: [Int] -> String
whatsInsideThisList []         = "It's empty!"
whatsInsideThisList [x]        = "A single element: " ++ show x
whatsInsideThisList [x, y]     = "Two elements: " ++ show x ++ " and " ++ show y
whatsInsideThisList (x:y:z:[]) = "The list has three elements: " ++ show [x,y,z]
whatsInsideThisList (x:rest)   = "The first element is: " ++ show x ++ ", and there are quite a few more!"

whatsInsideThisList []           -- "It's empty!"
whatsInsideThisList [1, 2]       -- "Two elements: 1 and 2"
whatsInsideThisList [1, 2, 3]    -- "The list has three elements: [1,2,3]"
whatsInsideThisList [1, 2, 3, 4] -- "The first element is: 1, and there are quite a few more!"
```

&lt;style&gt;/* Styles used for the Hoogle display in the pager */ .hoogle-doc { display: block; padding-bottom: 1.3em; padding-left: 0.4em; } .hoogle-code { display: block; font-family: monospace; white-space: pre; } .hoogle-text { display: block; } .hoogle-name { color: green; font-weight: bold; } .hoogle-head { font-weight: bold; } .hoogle-sub { display: block; margin-left: 0.4em; } .hoogle-package { font-weight: bold; font-style: italic; } .hoogle-module { font-weight: bold; } .hoogle-class { font-weight: bold; } .get-type { color: green; font-weight: bold; font-family: monospace; display: block; white-space: pre-wrap; } .show-type { color: green; font-weight: bold; font-family: monospace; margin-left: 1em; } .mono { font-family: monospace; display: block; } .err-msg { color: red; font-style: italic; font-family: monospace; white-space: pre; display: block; } #unshowable { color: red; font-weight: bold; } .err-msg.in.collapse { padding-top: 0.7em; } .highlight-code { white-space: pre; font-family: monospace; } .suggestion-warning { font-weight: bold; color: rgb(200, 130, 0); } .suggestion-error { font-weight: bold; color: red; } .suggestion-name { font-weight: bold; } &lt;/style&gt;<br>Use list literal pattern<br>Found:<br>(x : y : z : [])<br>Why Not:<br>[x, y, z]

```
"It's empty!"
"Two elements: 1 and 2"
"The list has three elements: [1,2,3]"
"The first element is: 1, and there are quite a few more!"
```

Như bạn thấy, có thể khớp mẫu cho:

- Danh sách trống `[]` .

- Danh sách kích thước cố định, cả với cách viết thông thường ( `[x]` , `[x,y]` ) và cách viết không syntactic sugar ( `x:[]` , `x:y:[]` ).

- Danh sách không rỗng có kích thước bất kỳ  `x:rest` . (Thường được sử dụng trong các hàm đệ quy và thường được viết là `x:xs` .)

<div class="alert alert-block alert-info"> Chúng ta dùng `()` bao quanh các mẫu của hai định nghĩa cuối cùng để chỉ ra rằng hàm lấy mọi thứ bên trong `()` làm một đối số duy nhất.</div>

Và vì chúng ta đã gán các kết quả khớp với các biến ( `x` , `y` , `z` , `rest` ), nên bạn có thể sử dụng các biến đó bên trong định nghĩa của hàm.

Nhưng nếu bạn không cần chúng thì sao? Nếu bạn chỉ cần thực hiện một hành động nào đó khi một mẫu được khớp, chứ không quan tâm đến giá trị thực tế thì sao?

**Việc gán các giá trị rồi bỏ qua chúng sẽ làm ô nhiễm môi trường của bạn với các biến mà bạn không bao giờ sử dụng!** Nhưng đừng lo lắng. Trong tình huống này, bạn có thể bỏ qua dữ liệu mà bạn không quan tâm trong khi khớp mẫu cho phần còn lại! Hãy xem hàm sau đây. Nó cho chúng ta biết đâu là phần tử đầu tiên và thứ ba trong danh sách `Bool` (nếu có):

```haskell
firstAndThird :: [Bool] -> String
firstAndThird (x:_:z:_) = "The first and third elements are: " ++ show x ++ " and " ++ show z
firstAndThird _ = "Don't have them!"

firstAndThird [True, True, False]
```

```
"The first and third elements are: True and False"
```

Định nghĩa đầu tiên sẽ khớp mẫu cho bất kỳ danh sách nào có 3 phần tử trở lên, trong khi `_` sẽ bỏ qua phần tử thứ hai và phần còn lại của danh sách.

Còn đối với bất kỳ danh sách nào khác không rơi vào trường hợp đầu, chúng ta chỉ đơn giản bỏ qua nó hoàn toàn với `_` cho toàn bộ danh sách đó.

Tuyệt vời phải không? Biết được điều này, chúng ta có thể sửa đổi hàm `initials` của bài học trước từ thế này:

```haskell
initials :: String -> String -> String
initials name lastName = if name == "" || lastName == ""
                         then "How was your name again?"
                         else let x = head name
                                  y = head lastName
                              in [x] ++ "." ++ [y] ++ "."

initials' "Nikola" "Tesla"
```

```
"N.T."
```

thành như này:

```haskell
initials' :: String -> String -> String
initials' (f:_) (l:_) = [f] ++ "." ++ [l] ++ "."
initials' _ _ = "How was your name again?"

initials' "Nikola" "Tesla"
```

```
"N.T."
```

Ngắn hơn và rõ ràng hơn.

Bây giờ hãy xem việc khớp mẫu giúp công việc của chúng ta dễ dàng hơn như thế nào với các tuple!

## Khớp mẫu với Tuple

Hãy nhớ lại ở các bài học trước, chúng ta chỉ có thể lấy các phần tử bên trong một pair (tuple gồm hai phần tử) bằng cách sử dụng hàm `fst` và `snd` .

Nếu cần lấy một giá trị từ các tuple lớn hơn thì bạn đang gặp khó khăn. 👀 Nhưng giờ đây, khi bạn đã là một bậc thầy về khớp mẫu 🪄, thì mọi thứ đều có thể!

Bạn muốn trích xuất phần tử đầu tiên của tuple 3 phần tử? Không vấn đề gì:

```haskell
firstOfThree :: (a, b, c) -> a
firstOfThree (x, _, _) = x

firstOfThree (1,2,3)
```

```
1
```

**Xong!**

Bạn muốn tạo một pair với phần tử thứ hai và thứ tư của bộ 4 phần tử? Giống như trên!:

```haskell
pairFromFour :: (a, b, c, d) -> (b, d)
pairFromFour (_, x, _, y) = (x, y)

pairFromFour (1,2,3,4)
```

```
(2,4)
```

**BÙM! 🔥 Xong rồi!** Và bạn có thể tiếp tục nếu muốn. Nhưng bây giờ chúng ta sẽ chuyển sang biểu thức `case` .

## Biểu thức <code>case</code>

Với biểu thức  `case` , chúng ta có thể thực thi một khối mã cụ thể dựa trên mẫu của một biến.

Tương tự như với câu lệnh `switch` trong các ngôn ngữ lập trình khác, biểu thức `case` có dạng như sau:

```haskell
case <Exp> of <Pattern1> -> <Result1>
              <Pattern2> -> <Result2>
              <Pattern3> -> <Result3>
	          ...
```

Trong đó giá trị của `<Exp>` được so sánh với mọi `<Pattern>` bên trong khối `of` . Và nếu nó khớp, `<Result>` tương ứng sẽ được tính toán.

(Lưu ý rằng không có dấu `=` ! Đó là vì toàn bộ biểu thức `case` chỉ là một biểu thức. Không phải là một hàm hay một phép gán.)

Ví dụ: chúng ta có thể viết một hàm nhận một bộ 3 số nguyên `Int` và kiểm tra xem có phần tử nào trong đó bằng 0 hay không:

```haskell
checkForZeroes :: (Int, Int, Int) -> String
checkForZeroes tuple3 = case tuple3 of
  (0, _, _) -> "The first one is a zero!"
  (_, 0, _) -> "The second one is a zero!"
  (_, _, 0) -> "The third one is a zero!"
  _         -> "We're good!"
  
checkForZeroes (32,0,256)
```

```
"The second one is a zero!"
```

Và tôi có thể nghe thấy bạn đang nói: "Kết quả cuối cùng chẳng phải giống với kết quả khi chúng ta khớp mẫu trên các tham số trong định nghĩa hàm sao?"

Vâng ... Đúng vậy. Về bản chất, khớp mẫu trên các tham số trong định nghĩa hàm chỉ là cú pháp gọn hơn cho biểu thức case! Vì vậy, đoạn code trên có thể thay thế bằng đoạn code này:

```haskell
checkForZeroes :: (Int, Int, Int) -> String
checkForZeroes (0, _, _) = "The first one is a zero!"
checkForZeroes (_, 0, _) = "The second one is a zero!"
checkForZeroes (_, _, 0) = "The third one is a zero!"
checkForZeroes _         = "We're good!"

checkForZeroes (32,0,256)
```

```
"The second one is a zero!"
```

Nhưng vì bây giờ chúng ta đang sử dụng BIỂU THỨC case, nên chúng ta có thể dùng chúng ở bất kỳ nơi nào có thể sử dụng biểu thức, không chỉ khi định nghĩa một hàm. Vì vậy, ví dụ: chúng ta có thể ghép kết quả đánh giá biểu thức case với một Chuỗi khác:

```haskell
checkForZeroes' :: (Int, Int, Int) -> String
checkForZeroes' tuple3 = "The " ++ show tuple3 ++ " has " ++
    case tuple3 of
      (0, _, _) -> "a zero as its first element"
      (_, 0, _) -> "a zero as its second element"
      (_, _, 0) -> "a zero as its third element"
      _         -> "no zeroes!"

checkForZeroes' (32,0,256)
```

```
"The (32,0,256) has a zero as its second element"
```

Điều đó làm cho các biểu thức `case` trở nên thuận tiện khi sử dụng bên trong các biểu thức khác. Ngoài ra, hãy nhớ rằng bất cứ điều gì bạn có thể làm được với biểu thức `case` đều có thể thực hiện bằng cách định nghĩa hàm với `let` , `where` hoặc Guards.

Và điều đó đặt ra câu hỏi: "Tại sao chúng ta có nhiều cách để làm cùng một việc như vậy?!" Tôi sẽ cho bạn biết tại sao...

## Phong cách khai báo (Declaration style) 🆚 Phong cách biểu thức (Expression style)

Có hai phong cách chính để lập trình hàm trong Haskell:

- **Phong cách khai báo** là cách bạn xây dựng thuật toán dưới dạng một hệ các phương trình cần thỏa mãn.
- **Phong cách biểu thức** là cách bạn xây dựng các biểu thức lớn từ các biểu thức nhỏ.

Nhiều mùa trăng trước đây, những các vị thần tạo ra Haskell đã tranh luận gay gắt xem phong cách nào tốt hơn. Chủ yếu là vì nếu có thể thì việc chỉ có một cách để làm việc gì đó sẽ ít gây nhầm lẫn và dư thừa hơn. Nhưng! Sau khi đổ máu, mồ hôi và nước mắt, họ quyết định hỗ trợ đầy đủ về mặt cú pháp cho cả hai. Và hãy để những người bình thường sử dụng những gì họ thích nhất.

Ví dụ về điều này, chúng ta có:

Phong cách khai báo | Phong cách biểu thức
--- | ---
Mệnh đề `where` | Biểu thức `let`
Khớp mẫu trong định nghĩa hàm: `f [] = 0` | Biểu thức case: `f xs = case xs of [] -> 0`
Các guard trong định nghĩa hàm: `f [x] | x > 0 = 'a'` | Biểu thức `if`: `f [x] if x > 0 then 'a' else...`
Đối số hàm ở phía bên trái: `fx = x*x` | Hàm lambda: `f = \x -> x*x`

Cái tên lambda ở cuối bảng là gì vậy? Đó là chủ đề của bài học tuần tới! 😁 Vậy nên hãy nhớ xem nhé!

Giờ là lúc tổng kết:

## Tổng kết

- Việc khớp mẫu cho các định nghĩa hàm giúp dễ dàng thực hiện những việc khác nhau tùy thuộc vào cấu trúc hoặc giá trị của các đối số.

- Khớp mẫu trên tuple, danh sách và các cấu trúc khác cho phép bạn dễ dàng trích xuất các giá trị chứa trong đó.

- Biểu thức case là một cách diễn đạt rõ ràng hơn các định nghĩa hàm dùng khớp mẫu nhưng chúng cũng có thể được sử dụng ở hầu hết mọi nơi như bất kỳ biểu thức nào khác. (Không chỉ để định nghĩa hàm.)

- Hai phong cách chính để lập trình hàm trong Haskell là "Phong cách khai báo" và "Phong cách biểu thức". Đừng phí thời gian tranh cãi xem cái nào là tốt nhất. Hãy áp dụng cái bạn thích hơn hoặc kết hợp chúng theo ý muốn.
