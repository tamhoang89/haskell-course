```haskell
:opt no-lint
```

# Cải thiện và kết hợp các hàm

## Nội dung

- Hàm bậc cao - `filter` - `any`
- Hàm Lambda
- Mức ưu tiên (Precedence) và tính kết hợp (associativity)
- Hàm curry - Curried functions
    - Áp dụng một phần - Partial application
- Áp dụng hàm và hàm hợp
    - Toán tử `$`
    - Toán tử `.`
- Phong cách Point-free

## Các hàm bậc cao

**Hàm bậc cao** là hàm nhận các hàm khác làm đối số hoặc trả về một hàm làm kết quả.

Bởi vì chúng ta có thể truyền các hàm làm đầu vào, trả về chúng như kết quả và gán chúng cho các biến, nên chúng giống như bất kỳ giá trị nào khác. Vì vậy, chúng ta nói rằng hàm là các **công dân hạng nhất** .

Hãy bắt đầu với một ví dụ điển hình. Hãy tưởng tượng rằng bạn có một hàm mà bạn thường áp dụng hai lần (vì lý do nào đó). Như thế này:

```haskell
complexFunc1 :: Int -> Int
complexFunc1 x = x + 1

func1 :: Int -> Int
func1 x = complexFunc1 (complexFunc1 x)

complexFunc2 :: Int -> Int
complexFunc2 x = x + 2

func2 :: Int -> Int
func2 x = (complexFunc2 (complexFunc2 x)) + (complexFunc2 (complexFunc2 x))
```

Đây là một ví dụ phóng đại, nhưng bạn có thể thấy một mẫu bắt đầu xuất hiện. Bạn luôn sử dụng `complexFunc1` và `complexFunc2` hai lần! Ngay khi chúng tôi nhìn thấy mẫu này, chúng tôi nhận ra có thể làm tốt hơn. Điều gì sẽ xảy ra nếu chúng tôi tạo một hàm nhận hai tham số - một hàm và một giá trị - và áp dụng hàm đó vào giá trị hai lần!

Chúng ta có thể thực hiện nó bằng đoạn code đơn giản:

```haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
```

Ở đây, chữ ký kiểu khác với chữ ký trước đó. Phần `(a -> a)` chỉ ra rằng tham số đầu tiên là một hàm nhận giá trị thuộc kiểu `a` và trả về giá trị cùng kiểu. Tham số thứ hai chỉ là giá trị thuộc kiểu `a` và toàn bộ hàm `applyTwice` trả về giá trị thuộc kiểu `a` .

Và trong phần thân của hàm, bạn có thể thấy rằng nó lấy tham số đầu tiên (hàm `f` ), áp dụng nó cho `x` và sau đó áp dụng lại `f` cho kết quả. Vì vậy chúng ta đang áp dụng hàm `f` hai lần.

Và thế là xong! Chúng ta đã tạo ra một hàm bậc cao!

Chúng ta có thể sử dụng hàm `applyTwice` để đơn giản hóa đoạn code bên trên như sau:

```haskell
func1' :: Int -> Int
func1' x = applyTwice complexFunc1 x

func2' :: Int -> Int
func2' x = (applyTwice complexFunc2 x) + (applyTwice complexFunc2 x)
```

Đây là một ví dụ đơn giản, nhưng hàm bậc cao là một tính năng cực kỳ mạnh mẽ. Dùng nhiều đến mức chúng ở khắp mọi nơi! Trên thực tế, bạn có thể tạo một Ngôn ngữ miền chuyên biệt (Domain Specific Language) của riêng mình bằng cách sử dụng các hàm bậc cao! Nhưng chúng ta hãy đi từng bước một. Bắt đầu bằng cách sử dụng hai hàm bậc cao có sẵn trong Haskell.

### Hàm `filter`

Hãy bắt đầu với hàm `filter` :

```haskell
:t filter
```

filter :: forall a. (a -&gt; Bool) -&gt; [a] -&gt; [a]

Hàm này nhận vào một hàm kiểm tra (hàm trả về một boolean) `a -> Bool` và một danh sách các phần tử thuộc kiểu `a` và lọc ra các phần tử của danh sách thỏa mãn hàm kiểm tra đó.

Ví dụ: nếu muốn lọc ra các số chẵn trong danh sách từ 1 đến 20, chúng ta có thể thực hiện như sau:

```haskell
filter even [1..20]
```

```
[2,4,6,8,10,12,14,16,18,20]
```

Hoặc, với một điều kiện phức tạp hơn, chúng ta có thể lọc từ danh sách các loại trái cây, chỉ lấy những loại có chứa chữ cái `'a'` :

```haskell
fruitWithA = filter tempFunct ["Apple", "Banana", "Pear", "Grape", "Wood"]
                where tempFunct x = 'a' `elem` x
fruitWithA
```

```
["Banana","Pear","Grape"]
```

Như bạn có thể thấy, bạn cũng có thể định nghĩa một hàm trong mệnh đề `where` để truyền nó làm hàm kiểm tra của `filter` .

### Hàm `any`

Chúng ta cũng có hàm `any`:

```haskell
-- Only for lists:  any :: (a -> Bool) -> [a] -> Bool
```

Hàm này cũng nhận vào một hàm điều kiện và một danh sách các phần tử. Nhưng nó sẽ kiểm tra xem có tồn tại **bất kỳ** phần tử nào trong danh sách mà hàm kiểm tra được thỏa mãn hay không.

Ví dụ: ở đây chúng ta đang kiểm tra xem có bất kỳ phần tử nào của danh sách lớn hơn 4 hay không. Nếu chỉ có một phần tử, `any` trả về `True` , nếu không, nó sẽ trả về `False` :

```haskell
biggerThan4 x = x > 4

any biggerThan4 [1,2,3,4]
```

```
False
```

Một bài toán thực tế hơn để sử dụng `any` là kiểm tra xem chúng ta có còn chiếc xe nào trên trang web bán xe của mình không:

```haskell
cars = [("Toyota",0), ("Nissan",3), ("Ford",1)]

biggerThan0 (_,x) = x > 0

any biggerThan0 cars
```

```
True
```

Trong `biggerThan0` , chúng ta khớp mẫu trên tuple để trích xuất số lượng ô tô và kiểm tra xem nó có lớn hơn 0 hay không. Sau đó, chúng ta sử dụng `any` để kiểm tra xem có cặp nào trong danh sách còn ít nhất một chiếc hay không.

Được rồi, chúng ta đã thấy rất nhiều ví dụ về hàm lấy các hàm khác làm tham số. Nhưng còn các hàm trả về hàm dưới dạng kết quả thì sao? Chúng ta sẽ cùng xem chúng. Nhưng trước tiên hãy cùng tìm hiểu về trừu tượng lambda và các hàm curry.

## Hàm Lambda

Thuật ngữ hàm lambda xuất phát từ hệ thống toán học gọi là **lambda calculus** . Bản thân nó là một chủ đề hấp dẫn và mạnh mẽ, nhưng hôm nay, chúng ta sẽ xem xét nó từ quan điểm của một lập trình viên thực tế.

Hàm lambda (còn gọi là hàm ẩn danh) là một định nghĩa hàm không có tên.

Ví dụ: đây là một hàm lambda nhận hai đối số và nhân chúng ( `f(x,y)=x*y` ) trong Haskell:

```haskell
\x y -> x * y
```

Hàm lambda bao gồm bốn phần:

1. Dấu gạch chéo ngược `\` ở đầu cho chúng ta biết rằng đây là hàm lambda.
2. Tên tham số (trong trường hợp này là `x`, `y` ) mà hàm nhận làm đầu vào.
3. Mũi tên ( `->` ) **phân tách** tham số đầu vào khỏi phần thân.
4. Và mọi thứ sau mũi tên là **phần thân** của hàm.

<div class="alert alert-block alert-info"> Hầu hết các ngôn ngữ lập trình hiện đại cũng đều có hàm ẩn danh. Nhưng không phải tất cả chúng đều hoạt động theo cùng một cách.</div>

### Tại sao bạn cần quan tâm?

Nghe có vẻ vô dụng vì làm thế nào bạn có thể sử dụng một hàm không có tên? Bạn không có cách nào để gọi nó sau này!

Trên thực tế, nó là một thành phần mạnh mẽ của ngôn ngữ! Thông qua khóa học này, chúng ta sẽ gặp nhiều tình huống ứng dụng thực tế của biểu thức lambda. Trước hết, bạn có thể sử dụng nó để tránh đặt tên cho các hàm mà bạn chỉ sử dụng một lần!

Bản thân điều này rất hữu ích, nhưng nó thực sự hiệu quả khi làm việc với các hàm bậc cao! Hãy xem lại ví dụ trước:

```haskell
biggerThan4 x = x > 4

any biggerThan4 [1,2,3,4]
```

```
False
```

Hàm `biggerThan4` đó sẽ không được sử dụng ở bất kỳ nơi nào khác nhưng nó sẽ tồn tại mãi mãi trong môi trường của chúng ta. Ngoài ra, đó là một hàm cực kỳ đơn giản! Tên hàm dài hơn thân hàm!

Bằng cách sử dụng biểu thức lambda, chúng ta có thể tạo và sử dụng `biggerThan4` làm tham số cho `any` cùng một lúc như sau:

```haskell
any (\x -> x > 4) [1,2,3,4]
```

```
False
```

Chúng ta cũng có thể sử dụng biểu thức lambda để đơn giản hóa các hàm khác. Hãy xem lại hàm `fruitWithA` :

```haskell
fruitWithA = filter tempFunct ["Apple", "Banana", "Pear", "Grape", "Wood"]
                where tempFunct x = 'a' `elem` x
fruitWithA
```

```
["Banana","Pear","Grape"]
```

Chúng ta có thể đơn giản hóa `fruitWithA` bằng cách loại bỏ `tempFunct` và thay thế nó bằng hàm lambda:

```haskell
filter (\x -> 'a' `elem` x) ["Apple", "Banana", "Pear", "Grape", "Wood"]
```

```
["Banana","Pear","Grape"]
```

Và tất nhiên, vì hàm lambda chỉ là biểu thức nên bạn có thể sử dụng chúng ở bất kỳ đâu có thể sử dụng biểu thức. Ngay cả với chính chúng:

```haskell
(\x -> x*2 + 1) 3
```

```
7
```

Nếu bạn cần thêm ví dụ, hãy tiếp tục theo dõi/đọc. Các hàm Lambda sẽ là một công cụ có giá trị để dễ dàng hình dung về khái niệm <strong>curry</strong>.

Bây giờ, chúng ta sẽ dành vài phút để tìm hiểu về mức độ ưu tiên và tính kết hợp.

## Mức ưu tiên và tính kết hợp

### Mức ưu tiên (Precedence)

Mức ưu tiên cho biết mức độ ưu tiên của toán tử (được biểu thị bằng số từ 0 đến 9). Nếu chúng ta sử dụng hai toán tử có mức ưu tiên khác nhau thì toán tử có mức ưu tiên cao hơn sẽ được áp dụng trước. Có nghĩa là các toán tử có quyền ưu tiên cao hơn sẽ liên kết chặt chẽ hơn!

Chúng ta có thể xem quyền ưu tiên cho một toán tử bằng lệnh info `:i` .

```haskell
:i (+)  -- infixl 6 +
:i (*)  -- infixl 7 *

1 + 2 * 3  -- Same as 1 + (2 * 3)
```

```
7
```

<div class="alert alert-block alert-info">     <code>infixl 6 +</code> và <code>infixl 7 *</code> được gọi là <b>khai báo fixity</b> .</div>

Vì phép nhân có mức ưu tiên là 7, cao hơn mức ưu tiên của phép cộng là 6 nên kết quả là 7 chứ không phải 9.

Và điều gì xảy ra khi hai toán tử có cùng mức độ ưu tiên? Đây là lúc tính liên kết phát huy tác dụng.

### Tính kết hợp (Associativity)

Khi chúng ta sử dụng lệnh `:i` ở trên, nó cũng trả về từ khóa `infixl` . Đây là tính kết hợp của toán tử.

Khi hai toán tử có cùng mức độ ưu tiên, tính kết hợp sẽ cho bạn biết bên nào (bên trái với `infixl` hoặc bên phải với `infixr` ) sẽ được tính toán trước.

Ví dụ:

- Các toán tử `(+)` và `(*)` có tính kết hợp trái, nghĩa là chúng tính toán vế trái trước.
- Toán tử `(:)` có tính kết hợp bên phải, nghĩa là nó tính toán vế phải trước tiên.
- Toán tử `(==)` không có tính kết hợp ( `infix` ), có nghĩa là nếu bạn sử dụng nhiều hơn một, bạn cần có dấu ngoặc đơn để biểu thị thứ tự.

```haskell
1 + 2 + 3 + 4  -- infixl: Same as ((1 + 2) + 3) + 4

1 : 2 : 3 : [] -- infixr: Same as 1 : (2 : (3 : []))

True == (False == False) -- infix: If you remove parenthesis, you'll get an error.
```

```
10

[1,2,3]

True
```

Và tất nhiên, bạn có thể thay đổi thứ tự tính toán bằng dấu ngoặc đơn:

```haskell
:i (**) -- infixr 8 **

2**3**4  -- infixr: Same as 2 ** (3 ** 4)
(2**3)**4
```

Cuối cùng, chúng ta có thể định nghĩa mức độ ưu tiên và tính kết hợp khi tạo toán tử của riêng mình. Như thế này:

```haskell
x +++ y = x + y -- Creating +++ operator
infixl 7 +++    -- Setting fixity of operator

1 +++ 2 * 3  -- 9
```

```
9
```

Bây giờ, kết quả là 9 vì `+++` và `*` đều liên kết trái và có cùng mức độ ưu tiên.

<div class="alert alert-block alert-info">
<b>Lưu ý quan trọng:</b>
   <ul>
       <li>Các toán tử không có khai báo fixity rõ ràng thì sẽ có fixity mặc định là <code>infixl 9</code>
</li>
       <li>Phép áp dụng hàm ("toán tử khoảng trắng") luôn có mức ưu tiên cao nhất (hãy tưởng tượng mức ưu tiên là 10).</li>
   </ul>
</div>

## Hàm curry - Curried functions

Currying là quá trình biến đổi một hàm sao cho thay vì nhận nhiều đầu vào, nó chỉ nhận một đầu vào và trả về một hàm nhận vào đầu vào thứ hai, và cứ tiếp tục như vậy.

Và đây là điều đặc biệt:

**Trong Haskell, tất cả các hàm đều được coi là curry! Nghĩa là, tất cả các hàm trong Haskell chỉ nhận một đối số!**

Để minh họa điều này, hãy xem hàm sau:

```haskell
add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z
```

Có vẻ như đây là một hàm nhiều tham số. Nhưng!, có những mối liên kết ẩn trong này! Chúng ta biết rằng phép áp dụng hàm ("toán tử khoảng trắng") luôn có mức ưu tiên cao nhất và liên kết trái, vì vậy nếu làm cho nó rõ ràng, chúng ta sẽ nhận được:

```haskell
add3 :: Int -> Int -> Int -> Int
((add3 x) y) z = x + y + z
```

Và nếu chúng ta kiểm tra fixity của hàm mũi tên ( `->` ):

```haskell
:i (->)  -- infixr -1 ->
```

Chúng ta thấy rằng nó liên kết với bên phải! Vì vậy, một cách rõ ràng hơn để viết chữ ký của hàm `add3` là:

```haskell
add3 :: Int -> (Int -> (Int -> Int))
((add3 x) y) z = x + y + z

add3 1 2 3
```

```
6
```

Điều này hoàn toàn tương đồng với định nghĩa của hàm! Tuy nhiên, để làm cho nó rõ ràng hơn, chúng ta sẽ diễn giải bằng cách sử dụng các hàm lambda.

Bắt đầu với định nghĩa trên:

```haskell
add3 :: Int -> (Int -> (Int -> Int)) -- Same as: add3 :: Int -> Int -> Int -> Int
((add3 x) y) z = x + y + z           -- Same as: add3 x y z = x + y + z
```

Chúng ta sẽ di chuyển từng tham số từ bên trái dấu `=` sang bên phải. Tạo cùng một hàm nhiều lần nhưng được viết khác nhau. Vì vậy, bắt đầu bằng `z` (tham số ngoài cùng), một hàm `add3` tương đương hoạt động giống hệt như hàm gốc có thể được viết như sau:

```haskell
add3 :: Int -> (Int -> (Int -> Int))
(add3 x) y = \z -> x + y + z
```

Bây giờ, `add3` là một hàm nhận hai số ( `x y` ) và trả về một hàm nhận một số khác ( `z` ) rồi cộng ba số đó lại với nhau.

Nếu chúng ta làm một lần nữa với giá trị thứ hai:

```haskell
add3 :: Int -> (Int -> (Int -> Int))
add3 x = \y -> (\z -> x + y + z)
```

Bây giờ, `add3` là một hàm nhận một số ( `x` ) và trả về một hàm nhận một số ( `y` ) trả về một hàm nhận một số ( `z` ) rồi cộng ba số đó lại với nhau.

Và nếu chúng ta làm điều đó một lần nữa:

```haskell
add3 :: Int -> (Int -> (Int -> Int))
add3 = \x -> (\y -> (\z -> x + y + z))
```

Chúng ta có `add3` là một name trả về một hàm nhận một số ( `x` ) và trả về một hàm nhận một số ( `y` ) trả về một hàm nhận một số ( `z` ) rồi cộng ba số lại với nhau.

Đó là một hành trình khá dài nhưng chúng tôi đã cố gắng làm cho currying trở nên rõ ràng!

Và bây giờ, cách chữ ký được viết đã có ý nghĩa hơn nhiều! Mỗi lần bạn thay thế một tham số, nó sẽ trả về một hàm mới như kết quả. Tiếp tục cho đến khi bạn thay thế tham số cuối cùng, cho bạn kết quả cuối cùng.

Và bởi vì `->` có tính kết hợp phải, chúng ta có thể loại bỏ các dấu ngoặc không cần thiết của cả chữ ký và định nghĩa để có được một mã sạch sẽ hơn:

```haskell
add3 :: Int -> Int -> Int -> Int
add3 = \x -> \y -> \z -> x + y + z
```

Và bây giờ, ví dụ, nếu chúng ta áp dụng hàm cho 3 tham số như thế này:

```haskell
add3 1 2 3
```

```
6
```

Đây là những gì diễn ra từng bước một (tôi thêm các dấu ngoặc đơn để dễ hình dung):

```haskell
add3 :: Int -> (Int -> (Int -> Int))
add3 = \x -> (\y -> (\z -> x + y + z))

---

add3 1 = \y -> (\z -> 1 + y + z)       :: Int -> (Int -> Int)

add3 1 2 = \z -> 1 + 2 + z             :: Int -> Int

add3 1 2 3 = 1 + 2 + 3                 :: Int
```

Vậy, ngoài việc là một câu chuyện thú vị để khởi đầu tại câu lạc bộ, điều này có ích như thế nào đối với bạn? Chà... với các hàm uncurry, nếu bạn cung cấp ít tham số hơn số tham số được yêu cầu, bạn sẽ nhận được một lỗi. Nhưng vì, trong Haskell, tất cả các hàm đều là curry, bạn có thể tận dụng nó để sử dụng "áp dụng một phần"!

### Áp dụng một phần - Partial application

Áp dụng một phần trong Haskell có nghĩa là bạn cung cấp ít đối số hơn số lượng tối đa mà hàm chấp nhận.

Kết quả (như chúng ta đã thấy trước đó) là một hàm mới chứa các tham số còn lại mà bạn chưa cung cấp cho hàm ban đầu.

Như một ví dụ thực tế về sự hữu ích của tính năng này, giả sử bạn có một hàm được sử dụng để tạo email ở định dạng `name.lastName@domain` . Các thông số bạn cung cấp là tên miền, tên và họ:

```haskell
createEmail :: String -> String -> String -> String
createEmail domain name lastName = name ++ "." ++ lastName ++ "@" ++ domain
```

Bây giờ, công ty của bạn có hai cộng đồng khách hàng có hai tên miền khác nhau. Bạn không muốn người dùng của mình nhập tên miền mỗi lần, vì vậy bạn tạo 2 hàm trong đó bạn áp dụng một phần cho tên miền của họ:

```haskell
createEmailTeckel :: String -> String -> String
createEmailTeckel = createEmail "teckel-owners.com"

createEmailSCL :: String -> String -> String
createEmailSCL = createEmail "secret-cardano-lovers.com"

createEmailTeckel "Robertino" "Martinez"
createEmailSCL "Vitalik" "Buterin"
```

```
"Robertino.Martinez@teckel-owners.com"

"Vitalik.Buterin@secret-cardano-lovers.com"
```

Lưu ý rằng việc này có thể thực hiện được vì tên miền là tham số đầu tiên trong hàm `createEmail` . Vì vậy, thứ tự của các đối số là quan trọng.

Nếu vì lý do nào đó, tham số bạn muốn áp dụng không phải là tham số đầu tiên và bạn không được phép viết lại hàm hiện có, bạn có thể tạo hàm trợ giúp:

```haskell
-- With partial application:

createEmailJohn :: String -> String -> String
createEmailJohn lastName domain = createEmail domain "John" lastName

-- Without partial application:

createEmail' :: String -> String -> String -> String
createEmail' name lastName domain = createEmail domain name lastName
```

Và vì các toán tử chỉ là các hàm infix nên chúng ta cũng có thể áp dụng một phần chúng!

Ví dụ, nhớ lại ví dụ trước về hàm bậc cao:

```haskell
any (\x -> x > 4) [1,2,3,4]
```

```
False
```

Trong hàm mà chúng ta truyền dưới dạng tham số, chúng ta cần so sánh xem đầu vào có lớn hơn `4` hay không. Và toán tử `>` là một hàm nhận vào hai tham số và so sánh xem tham số đầu tiên có lớn hơn tham số thứ hai hay không. Vì vậy chúng ta có thể áp dụng một phần tham số bên phải để có được kết quả tương tự:

```haskell
any (>4) [1,2,3,4]
```

```
False
```

Áp dụng một phần của toán tử trung tố được gọi là một *section* .

Và tôi không chắc bạn có để ý không, nhưng chúng ta vừa thay thế tham số thứ hai (tham số bên phải). Điều thú vị về các section là bạn có thể áp dụng một phần ở phía thuận tiện hơn:

```haskell
(++ "ing") "Think"     -- Same as \x -> x ++ "ing"

("Anti" ++) "library"  -- Same as \x -> "Anti" ++ x
```

```
"Thinking"

"Antilibrary"
```

<div class="alert alert-block alert-warning"> <b>Cảnh báo:</b> Toán tử <code>-</code> rất đặc biệt vì bạn không thể áp dụng một phần nó. <code>-1</code> được phân tích là số <code>-1</code> (âm 1) thay vì hiểu là toán tử <code>-</code> áp dụng một phần cho <code>1</code> . Hàm <code>subtract</code> có mặt để khắc phục vấn đề này.</div>

## Áp dụng và kết hợp các hàm

### Toán tử áp dụng hàm `$`

Nếu chúng ta kiểm tra cách định nghĩa toán tử áp dụng hàm trong Haskell, có vẻ hơi... lạ:

```
($) :: (a -> b) -> a -> b
f $ x =  f x
```

Chúng ta thấy rằng nó nhận một hàm `f` và một biến `x` rồi áp dụng hàm đó cho biến ( `f  x` ). Vì vậy, có vẻ như toán tử này là dư thừa vì nó hoạt động giống như một phép áp dụng hàm "khoảng trắng" thông thường ( `f x` ).

Và bạn biết gì không? Có một sự khác biệt nhỏ nhưng đáng kể giữa hai toán tử:

- Toán tử "khoảng trắng" có mức độ ưu tiên kết hợp trái cao nhất.
- Toán tử áp dụng hàm ( `$` ) có mức độ ưu tiên kết hợp phải thấp nhất: `infixr 0 $` .

Bạn có thể thấy sự khác biệt nếu chúng ta làm rõ điều này bằng dấu ngoặc đơn:

```
f g h x      = ((f g) h) x

f $ g $ h x  =  f (g (h x))
```

Để ví dụ về cách điều này thay đổi mọi thứ, hãy xem các biểu thức sau:

```haskell
(2 *) 3 + 4    -- Same as: ((2 *) 3) + 4
(2 *) $ 3 + 4  -- Same as: (2 *) (3 + 4)

max 5 4 + 2    -- Same as: ((max 5) 4) + 2
max 5 $ 4 + 2  -- Same as: (max 5) (4 + 2)
```

Như bạn có thể thấy trong các ví dụ trên, khi sử dụng `$`, toàn bộ biểu thức ở bên phải của nó sẽ được áp dụng làm tham số cho hàm ở bên trái. Vì vậy, bạn có thể thấy việc sử dụng `$` giống như đưa mọi thứ ở bên phải nó vào trong cặp dấu ngoặc đơn.

Điều này đưa chúng ta đến cách sử dụng chính của `$` : Bỏ dấu ngoặc đơn!

Trong biểu thức sau, có 3 cơ hội để loại bỏ dấu ngoặc đơn, vì vậy hãy loại bỏ chúng:

```haskell
-- All these expressions are equivalent:

show ((2**) (max 3 (2 + 2)))

show $ (2**) (max 3 (2 + 2))

show $ (2**) $ max 3 (2 + 2)

show $ (2**) $ max 3 $ 2 + 2
```

Điều này làm cho mã của bạn dễ đọc và dễ hiểu hơn.

Tất nhiên, bạn có thể làm nhiều việc hơn ngoài việc loại bỏ dấu ngoặc đơn, nhưng đó là điều bạn sẽ làm thường xuyên nhất. Vì vậy, chúng ta sẽ tạm để nó ở đó và bắt đầu tìm hiểu về toán tử hàm hợp ( `.` )!

### Phép kết hợp hàm (hàm hợp)

Chúng ta đã đề cập đến khái niệm hàm hợp trong bài học đầu tiên. Vì vậy, nếu bạn không chắc về nó, hãy xem lại! Tuy nhiên, xin nhắc lại và tóm tắt trong một vài từ:

Khi kết hợp hai hàm, chúng ta tạo ra một hàm mới tương đương với việc gọi hai hàm theo thứ tự khi hàm đầu tiên lấy đầu ra của hàm thứ hai làm đầu vào.

Chúng ta có thể làm điều này với dấu ngoặc đơn. Ở đây, hàm `f` lấy kết quả đầu vào của việc áp dụng hàm `g` cho `x` :

```haskell
f (g x)
```

Ví dụ dưới đây có vẻ hơi quá phức tạp, chúng ta có thể làm điều gì đó như thế này:

```haskell
complicatedF :: [Int] -> Bool
complicatedF x = any even (filter (>25) (tail ( take 10 x)))
```

Ở đây, chúng ta dùng hàm hợp khá nhiều! Chính xác là 3 lần! Và như bạn có thể thấy, đoạn code này khá khó đọc, vì vậy sơ đồ có thể giúp ích:

$$ \boxed{\mathrm{[Int]}} \xrightarrow{\mathrm{take\ 10}} \boxed{\mathrm{[Int]}} \xrightarrow{\mathrm{tail}} \boxed{\mathrm{[Int]}} \xrightarrow{\mathrm{filter\ (>25)}} \boxed{\mathrm{[Int]}} \xrightarrow{\mathrm{any\ even}} \boxed{\mathrm{Bool}} \quad = \quad \boxed{\mathrm{[Int]}} \xrightarrow{\mathrm{complicatedF}} \boxed{\mathrm{Bool}} $$


Chúng ta nhận danh sách `Int` làm đầu vào, sau đó sử dụng `take 10` để lấy 10 phần tử đầu tiên của danh sách, sau đó sử dụng kết quả làm đầu vào cho `tail` trả về 9 phần tử cuối cùng, sau đó sử dụng kết quả đó làm đầu vào cho `filter (>25)` để lọc các giá trị lớn hơn 25 và cuối cùng, lấy kết quả đó làm đầu vào cho `any even` để kiểm tra xem có số chẵn nào còn lại trong danh sách hay không.

Sơ đồ đã giúp ích, nhưng sẽ thế nào nếu tôi nói với bạn rằng có cách để cải thiện sự rõ ràng và dễ hiểu trong mã của chúng ta?

Điều này có thể được thực hiện bằng cách trừu tượng hóa hàm hợp cho một toán tử. Và bởi vì, trong toán học, ký hiệu hàm hợp là một chiếc nhẫn giống như một dấu chấm, nên chúng ta sẽ sử dụng dấu chấm:

```haskell
(.)  :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
infixr 9 .
```

Ở đây, chúng ta thấy rằng toán tử `.` nhận hai hàm ( `f :: b -> c` và `g :: a -> b` ) và kết hợp chúng bằng hàm lambda để chỉ ra rằng toàn bộ biểu thức `f . g` trả về một hàm nhận vào tham số `x :: a` , áp dụng `g` cho nó để nhận giá trị kiểu `b` và cuối cùng áp dụng `f` cho nó để nhận giá trị kiểu `c` .

Điều quan trọng cần lưu ý là `f` nhận một giá trị đầu vào có cùng kiểu với đầu ra của `g` . Vì vậy, hàm kết quả lấy đầu vào là một giá trị cùng kiểu với đầu vào của `g` ( `a` ) và trả về một giá trị cùng kiểu với đầu ra của `f` ( `c` ).

Vì vậy, bây giờ chúng ta có toán tử mới này, hàm `complicatedF` được viết lại thành:

```haskell
complicatedF :: [Int] -> Bool
complicatedF x = any even . filter (>25) . tail . take 10 $ x
```

Waaay dễ đọc hơn! Bạn có thể đọc ra mọi thứ hàm này thực hiện chỉ bằng một cái liếc mắt!

Ngoài ra, hãy lưu ý rằng mọi hàm ở cả hai phía của toán tử `.` nhận vào một đối số duy nhất hoặc được áp dụng một phần cho đến khi chỉ còn nhận một đối số duy nhất.

Nếu chúng ta viết lại ví dụ trước đó trong phần toán tử áp dụng hàm bằng cách sử dụng toán tử hàm hợp, chúng ta sẽ nhận được:

```haskell
show ((2**) (max 3 (2 + 2)))

show . (2**) . max 3 $ 2 + 2
```

Như bạn có thể thấy, `$` và `.` có thể làm cho mã của bạn rõ ràng và ngắn gọn. Nhưng hãy cảnh giác để không lạm dụng chúng! Bạn có thể sẽ nhận được một kết quả tồi tệ nhất!

Và bây giờ, một cách cuối cùng để làm cho các hàm dễ đọc hơn, thưa quý vị và các bạn, chúng tôi xin giới thiệu phong cách point-free!! 👏👏👏

### Phong cách point-free

Trong phong cách point-free (còn gọi là lập trình ngầm định), các định nghĩa hàm không khai báo các đối số.

Vì vậy, thay vì làm như này:

```haskell
fourOrLarger :: Int -> Int
fourOrLarger x = max 4 x

add1 :: Int -> Int
add1 x = 1 + x
```

Chúng ta có thể làm thế này:

```haskell
fourOrLarger :: Int -> Int
fourOrLarger = max 4

add1 :: Int -> Int
add1 = (1+)
```

Các hàm vẫn thực hiện công việc tương tự, nhưng hiện tại, chúng ta không liên kết đối số một cách rõ ràng và sử dụng nó bên trong phần thân hàm. Chúng được ẩn đi trong định nghĩa nhưng vẫn rõ ràng trong chữ ký.

Các hàm point-free có ưu điểm sau:

- Trở nên nhỏ gọn hơn.
- Dễ hiểu.
- Sạch hơn, vì chúng loại bỏ thông tin dư thừa.

Vậy chúng ta có thể sử dụng point-free để thay đổi hàm này:

```haskell
complicatedF :: [Int] -> Bool
complicatedF x = any even . filter (>25) . tail . take 10 $ x
```

thành:

```haskell
complicatedF :: [Int] -> Bool
complicatedF = any even . filter (>25) . tail . take 10
```

Nó cho chúng ta biểu thức cuối cùng của `complicatedF` .

Phong cách này đặc biệt hữu ích khi xây dựng các chương trình hiệu quả thông qua tính toán và nhìn chung, là một quy tắc tốt. Nó giúp người viết và người đọc suy nghĩ về việc kết hợp các hàm ở cấp độ cao thay vì xáo trộn dữ liệu ở cấp độ thấp.

Bài học hôm nay đến đây là kết thúc. Hôm nay chúng ta đã học rất nhiều khái niệm mới và cách cải thiện, kết hợp các hàm của mình. Có thể sẽ hơi khó hiểu nếu tiếp thu tất cả cùng một lúc, nhưng tất cả những khái niệm này đều quan trọng. Vì vậy, hãy chắc chắn rằng bạn hiểu chúng rõ ràng trước khi bước tiếp với khóa học.
