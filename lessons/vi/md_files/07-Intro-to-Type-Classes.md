# Giới thiệu về các lớp kiểu

## Nội dung

- Sự tuyệt vời của các lớp kiểu (type class)
- Lớp kiểu là gì
- Các lớp kiểu thông dụng
    - `Eq` , `Ord`
    - `Num` , `Integral` , `Floating`
    - `Read` , `Show`
- Kiểu dữ liệu hợp lệ tổng quát nhất
- Nhiều ràng buộc

<div class="alert alert-block alert-info">
<p>Bài học này sẽ giới thiệu về Lớp kiểu (Type Class) trong Haskell từ góc nhìn của người sử dụng. Nghĩa là, khi lập trình Haskell, bạn sẽ gặp các lớp kiểu và cần hiểu cách sử dụng chúng hiệu quả.</p>
<p>Trong hai bài tiếp theo, sau khi tìm hiểu về cách tạo kiểu dữ liệu, chúng ta sẽ xem xét từ góc độ của người tạo lớp kiểu và instance.</p>
</div>

## Sự tuyệt vời của các lớp kiểu

Cho đến nay, chúng ta đã biết rằng khi định nghĩa một hàm, ta có thể cho nó hoạt động với một kiểu dữ liệu cụ thể như thế này:

```haskell
sqr :: Int -> Int
sqr v = v * v
```

Hàm này đảm bảo tính an toàn cao vì nó chỉ nhận kiểu `Int` , bất kể giá trị nào được truyền vào, bạn có thể thực hiện phép toán với nó. Tuy nhiên, nhược điểm là bạn chỉ có thể sử dụng hàm này với kiểu đó. Nếu bạn cần nó cho `Double` hoặc `Float,` bạn phải định nghĩa lại hàm với tên khác như `sqrDouble` và `sqrFloat` .

Hoặc với kiểu đa hình như thế này:

```Haskell
fst :: (a, b) -> a
fst (x, _) = x
```

Điều này mang lại sự linh hoạt vì bạn có thể sử dụng các giá trị thuộc bất kỳ kiểu nào làm đầu vào, nhưng lại mất đi tính an toàn khi sử dụng kiểu dữ liệu cụ thể.

Vì vậy, chúng ta có một cái gì đó như thế này:

```
                    ↑
                    | X (Polym.)
                    |
      Flexibility   |
                    |
                    |
                    |                X (Types)
                    |
                    ------------------⟶
                             Safety
```

Lớp kiểu là những gì bạn nhận được khi bạn là một nhà phát triển cứng đầu, vừa muốn có tính linh hoạt của kiểu đa hình, cùng với tính an toàn của việc sử dụng kiểu.

```
                    ↑
                    | X (Polym.)     X (Type Classes)
                    |
      Flexibility   |
                    |
                    |
                    |                X (Types)
                    |
                    ------------------⟶
                             Safety
```

Cuối cùng, những gì Lớp kiểu làm là cho phép bạn sử dụng **các giá trị đa hình bị hạn chế** . Các giá trị có thể có nhiều kiểu khác nhau, nhưng không phải tất cả, chỉ là một tập hợp con được phép. Điều này được gọi là *đa hình Ad-hoc (Ad-hoc polymorphism)* hoặc *overloading* , nhưng bạn không cần phải nhớ chúng ngay bây giờ.

Bây giờ chúng ta đã biết tại sao lớp kiểu lại tuyệt vời, hãy xem chúng thực sự là gì!

## Lớp kiểu là gì?

Nếu bạn gặp những người thuộc câu lạc bộ vẽ trình độ cao, bạn sẽ biết họ có thể vẽ. Tại sao? Bởi vì đó là một trong những yêu cầu để vào câu lạc bộ!

Lớp kiểu giống như một câu lạc bộ mà các kiểu dữ liệu có thể tham gia nếu chúng có các hành vi (behavior) cụ thể. (Hành vi, trong ngữ cảnh này, có nghĩa là hàm). Vì vậy, một lớp kiểu quy định một loạt các hàm, và mỗi kiểu dữ liệu thuộc về lớp kiểu đó đưa ra cách định nghĩa riêng cho các hàm này.

Vì vậy, từ quan điểm của người lập trình sử dụng các kiểu và lớp kiểu đã tồn tại:

**Nếu bạn thấy một kiểu dữ liệu là Thể hiện (Instance) của một lớp kiểu, bạn biết rằng nó triển khai và hỗ trợ các hành vi (các hàm) của lớp kiểu đó.**

Ví dụ, kiểu `Bool` . Để xem các lớp kiểu mà kiểu `Bool` thuộc về, bạn có thể sử dụng lệnh `:i` (thông tin) trong ghci. Nếu chạy `:i Bool` , chúng ta nhận được:

```haskell
type Bool :: *
data Bool = False | True
  	-- Defined in ‘GHC.Types’
instance Eq Bool -- Defined in ‘GHC.Classes’
instance Ord Bool -- Defined in ‘GHC.Classes’
instance Enum Bool -- Defined in ‘GHC.Enum’
instance Show Bool -- Defined in ‘GHC.Show’
instance Read Bool -- Defined in ‘GHC.Read’
instance Bounded Bool -- Defined in ‘GHC.Enum’
```

Chúng ta sẽ tìm hiểu về `type` và `data` trong bài học tiếp theo. Vì vậy, nếu bỏ qua hai dòng mã đầu tiên, chúng ta sẽ thấy một loạt dòng có nội dung `instance ...` . Những dòng đó cho chúng ta biết rằng kiểu `Bool` là một instance của lớp kiểu `Eq` , lớp kiểu `Ord` , v.v.

Vì vậy, `Bool` triển khai các hàm của tất cả các lớp kiểu đó. Và, một cách tự nhiên, bây giờ chúng ta muốn biết các hành vi mà những lớp kiểu đó xác định. Hãy cùng tìm hiểu!

## Các lớp kiểu thông dụng

Bây giờ, chúng ta sẽ tìm hiểu về các lớp kiểu thông dụng nhất. Tôi sẽ cho bạn biết chúng đại diện cho điều gì và các hành vi chính của chúng. Nhưng bạn không cần phải ghi nhớ bất cứ điều gì về chúng. Sau bài học về "tạo lớp kiểu", bạn sẽ có thể nhanh chóng kiểm tra lại tất cả những gì tôi nói trong bài học này. Ngoài ra, đừng lo lắng về các chi tiết ngay bây giờ. Chúng ta sẽ dành bài học này và hai bài học nữa để tìm hiểu về hệ thống kiểu. Hãy sử dụng bài học này để bắt đầu phát triển ý tưởng về lớp kiểu và làm quen với những lớp kiểu tích hợp thông dụng nhất (thường là những lớp duy nhất bạn cần).

### Lớp kiểu `Eq`

Lớp kiểu `Eq` liên quan đến phép so sánh bằng nhau. Các kiểu dữ liệu là instance của lớp kiểu `Eq` có thể xác định xem hai giá trị thuộc kiểu đó có bằng nhau hay không bằng cách sử dụng các hàm `==` (bằng nhau) và `/=` (khác nhau).

Và vì kiểu `Bool` là một instance của `Eq` , nên chúng ta biết rằng có thể sử dụng hai hàm trên để so sánh các giá trị thuộc kiểu đó:

```haskell
True == False  -- False

True /= False  -- True
```

Và nếu kiểm tra chữ ký của hàm `==` và `/=` , chúng ta sẽ thấy một vài điểm mới:

```haskell
(==) :: Eq a => a -> a -> Bool

(/=) :: Eq a => a -> a -> Bool
```

Ký hiệu `=>` là ký hiệu **ràng buộc lớp** . Nó chỉ ra rằng **một kiểu đa hình bị ràng buộc phải là một instance của một lớp kiểu** .

Mã ở bên phải mũi tên dày ( `=>` ) là cùng một chữ ký kiểu mà chúng ta đã sử dụng cho đến nay. Và mã bên trái của mũi tên ( `=>` ) biểu thị các ràng buộc của lớp.

Trong trường hợp này, mã bên phải của mũi tên dày ( `a -> a -> Bool` ) chỉ ra rằng các hàm này nhận hai giá trị đa hình và trả về `Bool` . Giống như mọi khi. Và mã ở bên trái mũi tên dày ( `Eq a` ) chỉ ra rằng kiểu `a` được sử dụng hai lần ở bên phải mũi tên, phải là một instance của lớp kiểu `Eq` .

Vì vậy, chúng ta đang ràng buộc (giới hạn) các kiểu dữ liệu bạn có thể truyền vào hai hàm này, từ tất cả các kiểu sang chỉ những kiểu là instance của Lớp kiểu `Eq` .

Và không dừng lại ở đó. Ví dụ: hãy tưởng tượng bạn tạo hàm này:

```haskell
func x y = if x == y then x else y
```

Bạn không làm toán hoặc thao tác với chuỗi. **Nhưng bạn kiểm tra xem các giá trị có bằng nhau không. Vì vậy, bạn muốn đảm bảo rằng hàm này chỉ chấp nhận các giá trị có thể kiểm tra bằng nhau** . Đó là lý do tại sao cần có ràng buộc lớp kiểu `Eq` . Để ngăn bạn sử dụng các kiểu có các giá trị không thể so sánh.

Và bởi vì `==` có ràng buộc `Eq a` và `func` sử dụng `==` bên trong, Haskell đủ thông minh để suy ra rằng chữ ký kiểu hàm của chúng ta cũng có ràng buộc đó:

```haskell
func :: Eq a => a -> a -> a
func x y = if x == y then x else y
```

Bây giờ là lúc kiểm tra xem chúng ta có thể áp dụng các hàm này cho bao nhiêu kiểu dữ liệu. Chúng ta biết rằng có thể áp dụng nó cho `Bool` vì `Bool` là một instance của `Eq` . Nhưng còn gì nữa? Các instance khác là gì?

Nếu bạn sử dụng lệnh `:i Eq` , bạn sẽ thấy một danh sách dài các kiểu là các instance của lớp kiểu này:

```haskell
-- ...
instance Eq a => Eq [a] -- Defined in ‘GHC.Classes’
instance Eq Word -- Defined in ‘GHC.Classes’
instance Eq Ordering -- Defined in ‘GHC.Classes’
instance Eq Int -- Defined in ‘GHC.Classes’
instance Eq Float -- Defined in ‘GHC.Classes’
instance Eq Double -- Defined in ‘GHC.Classes’
instance Eq Char -- Defined in ‘GHC.Classes’
instance Eq Bool -- Defined in ‘GHC.Classes’
-- ... more instances
```

Như bạn thấy, tất cả các kiểu mà chúng ta đã gặp cho đến nay (và nhiều hơn nữa) là các instance của lớp kiểu này (trừ các hàm). Đó là lý do tại sao chúng ta có thể kiểm tra xem hai giá trị của kiểu `Char` , `Int` , `Float` , v.v. có bằng nhau hay không và đó là lý do tại sao chúng ta có thể áp dụng hàm `func` mà chúng ta vừa định nghĩa cho bất kỳ giá trị nào trong số chúng:

```haskell
func True False -- False

func 1 2        -- 2

func 1.0 1.0    -- 1.0

func 'a' 'c'    -- 'c'
```

Và nếu bạn vô tình truyền một giá trị không phải là instance của `Eq` , chẳng hạn như một hàm:

```haskell
f1 x = x + 1
f2 x = x + 2 - 1

func f1 f2
```

Bạn sẽ gặp lỗi:

```haskell
No instance for (Eq (Integer -> Integer))
        arising from a use of ‘==’
        (maybe you haven't applied a function to enough arguments?)
```

Bởi vì, như trong thông báo lỗi, kiểu `Integer -> Integer` không phải là một instance của `Eq`, trong khi chúng ta cần nó vì chúng ta đang sử dụng `==` .

Nó thực sự thú vị, nhưng bạn không thể làm được gì nhiều với các kiểu chỉ thuộc lớp kiểu `Eq` . Bạn chỉ có thể biết hai giá trị có bằng nhau hay không. Chỉ có thế. May mắn thay, `Eq` không phải là câu lạc bộ duy nhất trong thị trấn Haskell!

### Lớp kiểu `Ord`

Lớp kiểu `Ord` liên quan đến việc sắp thứ tự. Các kiểu dữ liệu là instance của lớp kiểu `Ord` có thể sắp xếp các giá trị của chúng và cho biết giá trị nào là lớn nhất.

Và để làm được điều đó, lớp `Ord` có tất cả các hàm sau:

```haskell
  (<), (<=), (>=), (>) :: Ord a => a -> a -> Bool
  max, min             :: Ord a => a -> a -> a
  compare              :: Ord a => a -> a -> Ordering
```

Chúng ta đã sử dụng các toán tử bất đẳng thức ( `<` , `>` , `<=` , `>=` ) trong các bài học trước. Chúng lấy hai giá trị cùng kiểu thuộc lớp kiểu `Ord` và trả về một boolean:

```haskell
4 > 9      -- False

'a' >= 'b' -- False
```

Và các giá trị được sắp xếp như thế nào? Nó phụ thuộc vào kiểu dữ liệu. Với các con số, nó tuân theo thứ tự toán học (ví dụ: `4` đứng trước `5` và sau `3` ). Với các ký tự, nó tuân theo thứ tự Unicode. Và các kiểu khác có cách sắp xếp khác. Như chúng tôi đã nói, mỗi kiểu thuộc về một lớp kiểu đều có cách triển khai (định nghĩa) riêng cho các hàm này. Chúng ta sẽ tìm hiểu thêm khi tạo các instance của riêng mình.

Nhưng với khả năng sắp xếp thứ tự, chúng ta có thể làm nhiều hơn là chỉ so sánh không bằng.

#### Hàm `min` và `max`

Hàm `min` nhận hai giá trị thuộc một kiểu là instance của `Ord` và trả về giá trị nhỏ nhất trong hai giá trị đó:

```haskell
min :: Ord a => a -> a -> a
```

Ví dụ:

```haskell
min 12 19 -- 12
```

Hàm `max` nhận hai giá trị thuộc một kiểu là instance của `Ord` và trả về giá trị lớn nhất của hai giá trị đó:

```haskell
max :: Ord a => a -> a -> a
```

Ví dụ:

```haskell
max 12 19 -- 19
```

#### Hàm `compare`

Hàm `compare` nhận hai giá trị thuộc một kiểu là instance của `Ord` và trả về một giá trị thuộc kiểu `Ordering` , cho biết thứ tự của các giá trị.

```haskell
compare :: Ord a => a -> a -> Ordering
```

Giống như kiểu `Bool` chỉ có hai giá trị ( `True` và `False` ), kiểu `Ordering` chỉ có ba giá trị: `LT` (lesser than - nhỏ hơn), `EQ` (equal - bằng) và `GT` (greater than - lớn hơn).

Ví dụ:

```haskell
compare 4 9         -- LT (4 is lesser than 9)

'f' `compare` 'e'   -- GT ('f' is greater than 'e')

True `compare` True -- EQ ( True is equal to True)
```

Một lần nữa, cho đến nay, tất cả các kiểu mà chúng ta đã học đều là instance của lớp kiểu này (trừ các hàm).

Bây giờ, bạn có thể nói: "Nếu tôi có thể kiểm tra bằng nhau bằng lớp kiểu `Ord` , tại sao tôi lại cần lớp kiểu `Eq` ?"

Đôi khi một kiểu phải là instance của một lớp kiểu trước để được phép trở thành instance của một lớp kiểu khác. Giống như bạn phải tham gia câu lạc bộ vẽ nguệch ngoạc trước khi được phép nộp đơn vào câu lạc bộ vẽ tranh.

Đó là trường hợp của `Eq` và `Ord` .

Để sắp xếp các giá trị của một kiểu, trước hết bạn phải có khả năng xác định xem chúng có bằng nhau hay không. Điều này cho chúng ta biết rằng nếu chúng ta có một kiểu là instance của `Ord` , thì nó cũng hỗ trợ tất cả các hành vi `Eq` ! Trong những trường hợp này, chúng ta nói rằng `Eq` là lớp cha (superclass) của `Ord` (ngược lại, `Ord` là lớp con (subclass) của `Eq` ).

Một lần nữa, bạn không cần phải ghi nhớ tất cả những điều này. Ban đầu, bạn có thể kiểm tra nhanh chúng và sau một chút thời gian, bạn sẽ thuộc lòng tất cả các hành vi và lớp con.

Điều tương tự cũng xảy ra với các lớp kiểu số.

### Lớp kiểu `Num`

Kiểu số là một trong những kiểu được sử dụng nhiều nhất trong bất kỳ ngôn ngữ lập trình nào. Nhưng không phải tất cả các kiểu số đều có thể làm được những việc giống nhau.

Các kiểu là instance của lớp kiểu `Num` có thể hoạt động giống như các số. Nhưng không giống như một tập hợp con cụ thể của các số. Lớp kiểu `Num` xác định các hành vi mà tất cả các số phải có.

Ví dụ, các kiểu dữ liệu là instance của lớp kiểu này có thể được cộng, trừ, hoặc nhân (cùng với các hành vi khác) :

```haskell
(+) :: Num a => a -> a -> a

(-) :: Num a => a -> a -> a

(*) :: Num a => a -> a -> a
```

Ví dụ:

```haskell
5 - 1      -- 4

8.9 + 0.1  -- 9.0

'a' - 'b'  -- ERROR! Char is not an instance of Num!

```

Bây giờ chúng ta sẽ nói về vấn đề chính! Hãy tưởng tượng tôi muốn tạo một hàm thực hiện một số phép toán:

```haskell
add1 x = x + 1
```

Tôi không muốn chọn một kiểu cụ thể như `Int` và chỉ cho phép giá trị `Int` . Các kiểu `Float` , `Double` và `Integer` hoàn toàn có thể hoạt động tốt! Tuy nhiên, nếu không có ràng buộc nào, tôi có thể truyền vào bất kỳ kiểu nào! Kết quả của `'a' + 'b'` là gì? Hoặc `True + False` ? Nó không có ý nghĩa gì cả!

Vì chỉ các kiểu dữ liệu là instance của lớp kiểu `Num` mới có thể sử dụng `+` và vì `Float` , `Double` , `Int` và `Integer` đều là instance của `Num` , nên chúng ta có thể ràng buộc hàm của mình như thế này:

```haskell
add1 :: Num a => a -> a
add1 x = x + 1
```

Nhưng hãy nhớ rằng nếu bạn không chắc chắn về chữ ký kiểu, hãy hỏi trình biên dịch! Nó biết rằng để sử dụng `+` , bạn phải là một instance của lớp kiểu `Num` , do đó, nó tự động suy ra chữ ký kiểu của `add1` ! Cung cấp sự linh hoạt đồng thời bảo vệ chúng ta cùng một lúc.

Điều này thật tuyệt. Nhưng đôi khi chúng ta cần một cái gì đó cụ thể hơn.

### Lớp kiểu `Integral`

Lớp kiểu `Num` bao gồm tất cả các số, nhưng lớp kiểu `Integral` chỉ có các số nguyên (toàn bộ). Chẳng hạn như `4` , nhưng không phải `4.3` .

`Integral` là một câu lạc bộ độc quyền hơn `Num` . Trong tất cả các kiểu chúng ta đã thấy cho đến nay, chỉ có `Int` và `Integer` thuộc về nó.

Lớp kiểu này quy định nhiều hành vi, một trong những hàm `Integral` phổ biến nhất là `div` .

```haskell
div :: Integral a => a -> a -> a
```

Nó nhận hai giá trị của một kiểu là instance của`Integral` và chia chúng, chỉ trả về phần nguyên của phép chia.

Ví dụ:

```haskell
3 `div` 5    -- 0

div 5 2      -- 2
```

Ngoài ra, chúng ta có lớp kiểu `Fractional` .

### Lớp kiểu `Fractional`

Lớp kiểu `Fractional` liên quan đến các số thập phân. Các kiểu dữ liệu là instance của lớp kiểu  `Fractional` có thể biểu diễn và xử lý các giá trị số thập phân.

Hàm được sử dụng nhiều nhất trong lớp kiểu  `Fractional` là `/` :

```haskell
(/) :: Fractional a => a -> a -> a
```

Đây là phép chia thông thường. Không giống như `div` , chúng ta có thể tính toán chính xác hơn các giá trị của mình vì chúng ta đang sử dụng số thập phân. Và chỉ `Float` và `Double` là các instance của lớp kiểu này.

Ví dụ:

```haskell
10 / 5  -- 2.0

5  / 2  -- 2.5

10 / 3  -- 3.3333333333333335
```

Lưu ý rằng chúng ta chưa bao giờ phải chỉ định kiểu của các giá trị số trong bất kỳ ví dụ nào cho đến nay. Đó là vì, ví dụ, số `3` có thể là một giá trị thuộc kiểu `Int` , `Integer` , `Float` , `Double` và bằng cách áp dụng các hàm nhất định, như `/` , trình biên dịch có thể xác định được rằng chúng ta muốn nói đến giá trị `3` thuộc một trong các kiểu là instance của`Fractional` .

```haskell
:t (10/3) -- (10/3) :: Fractional a => a
```

### Lớp kiểu `Show`

Lớp kiểu `Show` được sử dụng để chuyển đổi các giá trị thành `String` có thể đọc được. Nó có 3 hành vi khác nhau, nhưng hành vi bạn sẽ thấy nhiều nhất là hàm `show` :

```haskell
show :: Show a => a -> String
```

Hàm `show` trả về một biểu diễn `String` của bất kỳ kiểu nào là instance của lớp kiểu `Show` . Ví dụ:

```haskell
show (3 :: Int) -- "3"

show True       -- "True"
```

Hàm này thực sự hữu ích cho việc gỡ lỗi và in logs.

### Lớp kiểu `Read`

Lớp kiểu `Read` cung cấp hành vi ngược lại với lớp kiểu `Show` . Nghĩa là nó nhận một `String` và trả về một giá trị thuộc kiểu mà chúng ta yêu cầu, nếu có thể. Hành vi được sử dụng nhiều nhất là hàm  `read` :

```haskell
read :: Read a => String -> a
```

Ví dụ:

```haskell
read "3" / 2  -- 1.5

read "True" || False  -- True

read "[1,2,3]" :: [Int]  -- [1,2,3]
```

Hãy nhớ rằng nếu `String` không chứa giá trị hợp lệ hoặc hàm `read` không biết kiểu cần trả về, thì nó sẽ ném ra một ngoại lệ:

```haskell
read "3" -- Doesn't know which numeric type. Exception.

read "Turue" :: Bool -- "Turue" is not a valid Bool value. Exception.
```

Bạn có thể tìm thấy mô tả chi tiết về một lớp kiểu nếu bạn tìm kiếm nó trên Hoogle: https://hoogle.haskell.org/.

Bây giờ, chúng ta hãy xem trình biên dịch suy luận kiểu như thế nào.

## Kiểu dữ liệu hợp lệ tổng quát nhất

Chữ ký của hàm này là gì?

```haskell
fToC x = (x - 32)*5/9
```

Hàm `fToC` có thể có một số kiểu khác nhau. `fToC :: Float -> Float` chẳng hạn.

Nhưng trong quá trình suy luận kiểu, trình biên dịch không giả định gì và ràng buộc kiểu của hàm càng ít càng tốt. Cung cấp cho bạn ràng buộc tổng quát nhất.

Hãy thực hiện từng bước một.

Trong trường hợp này, hàm nhận một giá trị và trả về một giá trị. Vì vậy, chữ ký chung nhất sẽ là:

```haskell
fToC :: a -> a  -- Intermediate step
```

Tuy nhiên, giá trị mà nó nhận phải là kiểu số (chúng ta đang áp dụng một số hàm toán học. `-` , `*` và `/` ).

Nhưng kiểu nào? `Num` (vì phép `-` và `*` ) hay `Fractional` (vì phép `/` )?

Trong trường hợp này, tất cả các kiểu số đều là một phần của `Num`, nhưng chỉ `Float` và `Double` là một phần của `Fractional` . Vì vậy, để đảm bảo hàm này luôn hoạt động, nó phải nhận lớp kiểu có phạm vi hẹp nhất, nghĩa là `Fractional` :

```haskell
fToC :: Fractional a => a -> a
```

Và đó là cách trình biên dịch suy luận kiểu của biểu thức. Lưu ý rằng kiểu dữ liệu thậm chí có thể cụ thể hơn, như `Float -> Float` hoặc `Double -> Double` . Nhưng điều đó có nghĩa là bạn cần một ràng buộc kiểu chặt hơn mà không có lý do.

Cuối cùng, kiểu hợp lệ và tổng quát nhất sẽ chiến thắng.

Được rồi, cho đến giờ, chúng ta đã thực hiện ràng buộc một kiểu bằng một lớp kiểu cụ thể. Và chúng ta biết rằng có thể có các lớp kiểu chuyên biệt hơn ( `Fractional` là lớp kiểu chuyên biệt hơn `Num` ).

Nhưng điều gì sẽ xảy ra nếu chúng ta cần một kiểu có một bộ những khả năng... cụ thể hơn?

## Nhiều ràng buộc

Đôi khi bạn cần các ràng buộc khác nhau cho các biến kiểu khác nhau.

Hoặc cùng một biến kiểu nhưng có nhiều ràng buộc. Tất cả điều này có thể được thực hiện dễ dàng trong Haskell.

### Nhiều ràng buộc cho cùng một biến kiểu

Hãy xem hàm này, nó bỏ qua số 3 khi bắt gặp:

```haskell
skip3 x = if x == 3 then x+1 else x
```

`x` có thể thuộc bất kỳ kiểu nào là instance của `Eq` (vì `==` ) và `Num` (vì `+` và vì chúng ta đang so sánh đầu vào với giá trị `3` thuộc lớp kiểu `Num` ).

Để chỉ định nhiều ràng buộc cho cùng một biến kiểu, chúng ta phải đặt chúng trong dấu ngoặc đơn và thêm dấu phẩy vào giữa chúng.

Giống như cách viết một tuple:

```haskell
skip3 :: (Eq p, Num p) => p -> p
```

Bây giờ biến kiểu `p` phải là một instance của cả `Eq` và `Num` . Và tất nhiên, chúng ta có thể thêm nhiều ràng buộc hơn nếu cần.

### Ràng buộc cho nhiều biến kiểu

Hãy tạo một hàm nhận hai giá trị và trả về `1` nếu giá trị thứ nhất lớn hơn giá trị thứ hai và `0` nếu ngược lại:

```haskell
isXBigger x y = if x > y then 1 else 0
```

Trong trường hợp này, `x` và `y` phải là phiên bản của `Ord` . Và giá trị trả về là một số thuộc loại không xác định, do đó, đây là phiên bản `Num` tổng quát hơn.

Đặt chúng lại với nhau, chữ ký kiểu sẽ là:

```haskell
isXBigger :: (Ord a, Num p) => a -> a -> p
```

Bây giờ chúng ta sẽ luyện tập một chút. Hãy xem hàm này:

```haskell
mistery1 x y z = if x > y then z/2 else z
```

Chúng ta so sánh `x` và `y` với `>` , vì vậy chúng phải là các instance của lớp kiểu `Ord` .

Và giá trị trả về được chia bằng cách sử dụng `/` trong một trong các nhánh if-else. Vì vậy `z` phải là một instance của `Fractional` .

```haskell
mistery1 :: (Ord a, Fractional p) => a -> a -> p -> p
```

Và ví dụ cuối cùng của chúng ta là một sửa đổi nhỏ của `mistery1` trong đó chúng ta cộng thêm `1` vào `x` trước khi so sánh nó với `y` :

```haskell
mistery2 x y z = if x+1 > y then z/2 else z
```

Giống như trước. Nhưng bây giờ `x` và `y` cũng phải là một instance của `Num` để có thể sử dụng `+` :

```haskell
mistery2 :: (Ord a, Num a, Fractional p) => a -> a -> p -> p
```

Như bạn có thể thấy, **chúng ta có thể áp dụng nhiều ràng buộc tùy ý** .

Trong thực tế hàng ngày, trình biên dịch có thể suy luận chúng cho bạn (hầu hết thời gian). Nhưng bạn vẫn cần phải nhận thức về những gì đang diễn ra để diễn giải và hiểu chúng một cách chính xác. Ngoài ra, việc viết kiểu dữ liệu của hàm trước khi định nghĩa nó là một thói quen tốt và là cách tuyệt vời để giúp bạn dễ dàng hơn khi viết định nghĩa hàm sau này.

# Đó là nội dung bài hôm nay!
