# Kiểu dữ liệu, Chữ ký kiểu và Tính đa hình

## Nội dung

- Giới thiệu về kiểu dữ liệu
- Chữ ký kiểu của hàm
- Làm việc với hàm
    - Các biến trong Haskell
    - Hàm trung tố và tiền tố
- Các kiểu dữ liệu phổ biến
- Giá trị đa hình và biến kiểu
- Vui vẻ với danh sách!

## Giới thiệu về kiểu dữ liệu

### Toán tử `::`

Kiểu dữ liệu là một nhãn gắn cho mỗi biểu thức, đưa ra những ràng buộc về cách biểu thức được sử dụng.

Chúng ta sử dụng *dấu hai chấm đôi* `::` để hiển thị hoặc chỉ định kiểu cho biểu thức. Ví dụ:

```haskell
myexpression :: MyType
```

cho chúng ta biết biểu thức `myexpression` có kiểu `MyType` .

### Các kiểu thường dùng

Dưới đây là các kiểu tiêu chuẩn thường được sử dụng trong Haskel:

- `Int` và `Integer` cho số nguyên.
- `Float` và `Double` cho số thực dấu phẩy động.
- `Bool` cho giá trị logic `True` và `False` .
- `Char` cho ký tự.
- `String` cho chuỗi văn bản.

### Làm thế nào để kiểm tra kiểu?

Lệnh `:type` (hoặc viết tắt `:t` ) trong GHCI, theo sau là bất kỳ biểu thức hợp lệ nào, cho chúng ta biết kiểu của nó.

```haskell
:type True

:type False

:t (3 < 5)

:t 'A'

:t "Hello world!"
```

## Chữ ký của một hàm

Ký hiệu dấu hai chấm đôi `::` nên được đọc đơn giản là "thuộc kiểu" và biểu thị *chữ ký* kiểu. Hãy cùng xem chữ ký kiểu là gì bằng ví dụ sau. Trong Haskell, một hàm bình phương được định nghĩa như sau:

```haskell
square :: Int -> Int
square v = v * v
```

Dòng đầu tiên chứa **chữ ký - signature** , dòng thứ hai chứa **định nghĩa - definition** của hàm `square`.

- **Chữ ký** của một hàm là một thông điệp gửi tới toàn thế giới rằng hàm đó tồn tại, chỉ ra tên của nó và các kiểu mà nó làm việc.

- **Định nghĩa** của hàm mô tả chính xác những gì nó thực hiện.

Trong chữ ký

```haskell
square :: Int -> Int
```

có hai phần được *phân cách* bởi dấu hai chấm đôi:

- phần **tên** của hàm ở bên trái

- và **kiểu của hàm** ở bên phải.

**Tất cả dữ liệu trong chương trình Haskell đều thuộc một kiểu cụ thể.** Và vì hàm hoạt động với dữ liệu nên **chữ ký của nó chứa các kiểu đầu vào và đầu ra, được phân tách bằng các dấu mũi tên `->`** .

Chữ ký của hàm `square` cho chúng ta biết rằng nó chấp nhận một đối số *duy nhất* có kiểu `Int` và trả về một giá trị có cùng kiểu `Int`.

Nếu có nhiều hơn một đối số, thì chữ ký sẽ đơn giản được kéo dài ra. Ví dụ: chữ ký hàm `prod`, trả về tích của hai đối số nguyên, có thể trông như sau:

```haskell
prod :: Int -> Int -> Int
prod x y = x * y
```

Nó có hai đối số kiểu `Int` và đầu ra của nó cũng có kiểu `Int` .

Trong phần **định nghĩa** hàm, dấu `=` phân tách code thành hai phần:

- **Phần đầu** là mã ở bên trái của dấu `=`, gồm **tên của hàm** và **tên đối số** (tên, chứ không phải kiểu!), được phân tách bởi dấu cách.

- **Phần thân** là mã bên phải của dấu `=`, thể hiện bản chất của hàm, nội dung của nó.

### Chữ ký của hàm cho chúng ta biết điều gì?

Haskell là ngôn ngữ lập trình *hàm* và mọi chương trình đều bao gồm *các hàm*. Mỗi hàm nhận vào một vài tham số cố định thuộc một số kiểu và trả về một giá trị cũng có một kiểu. Ví dụ, hàm sau:

```haskell
not :: Bool -> Bool
```

nhận vào một tham số kiểu `Bool` và trả về giá trị phủ định của nó, cũng thuộc kiểu `Bool` .

Nhìn vào *dấu mũi tên `->` ngoài cùng bên phải* trong chữ ký, ta hiểu rằng:

- mọi thứ ở bên trái nó là **kiểu của các đối số**, cũng được phân tách bởi các dấu mũi tên `->` ,

- thứ ở bên phải là **kiểu giá trị trả về của hàm** .

## Làm việc với Hàm

### Biến trong Haskell (Names/definitions)

Hãy xem hàm này:

```haskell
name = "Bob"
```

Hàm này không có tham số, chúng ta có một hàm luôn trả về cùng một giá trị - một `String` - bất kể thế nào!

Vì vậy, chúng ta có một biểu thức kiểu như sau:

```haskell
name :: String
name = "Bob"
```

**Loại hàm không có tham số thường được gọi là một definition (định nghĩa) hoặc name (tên).**

Mặc dù vậy, bạn cũng có thể gọi nó là biến, vì đây là thứ hầu hết các ngôn ngữ lập trình gọi là biến. Tuy nhiên, chúng không phải lúc nào cũng có nghĩa giống nhau.

Bởi vì chúng ta không thể thay đổi giá trị của một định nghĩa (biểu thức ở bên phải của dấu `=` luôn cho một kết quả không đổi), `name` và `"Bob"` về cơ bản là giống nhau. Và chúng ta có thể sử dụng chúng thay thế cho nhau.

Khi nói về lập trình nói chung, một biến giống như một chiếc hộp chứa một giá trị. Và tên của biến được viết ở bên cạnh hộp. Bạn có thể đặt các giá trị vào bên trong hộp và - trong hầu hết các ngôn ngữ lập trình - sau này bạn có thể thay đổi ý định và thay thế giá trị bên trong hộp.

```haskell
-- THIS IS NOT VALID HASKELL!!!
x = 3
7 + x   -- 10
x = 5
7 + x   -- 12
```

OOOOOOOOhhhhhh nhưng không phải với Haskell, không, không, không! Một khi bạn nói với Haskell rằng `x` có nghĩa là `3` thì nó sẽ là `3` mãi mãi!

Về mặt kỹ thuật:

Các biến của Haskell là **bất biến - immutable.**

Như vậy khái niệm biến trong Haskell hoàn toàn khác biệt. Haskell có các biến, nhưng theo nghĩa toán học. Khi chúng ta nói:

```Haskell
x = 3
city = "Paris"
letter = 'a'
it'sTrue = True
```

Chúng ta đang tuyên bố rằng thuật ngữ ở bên trái dấu `=` có **thể hoán đổi** với thuật ngữ ở bên phải dấu `=` .

Và điều này cũng được áp dụng cho các tham số của hàm:

```haskell
volumeOfACylinder r h = pi * r^2 * h
```

Trong trường hợp này, khi chúng ta truyền các giá trị cho các tham số của `volumeOfACylinder` , chúng ta không thể thay đổi chúng bên trong phần thân hàm. Chúng ta có thể sử dụng lại hàm và truyền các giá trị khác cho tham số, nhưng chúng ta không thể *thay đổi* chúng một khi đã truyền chúng.

## Cách viết hàm trung tố và tiền tố

Bạn có thể gọi (sử dụng) hàm theo hai cách khác nhau: trung tố và tiền tố.

### Tiền tố

Hãy xem biểu thức sau:

```haskell
prod x y = x * y
prod 4  5
```

`prod` được sử dụng ở **dạng tiền tố** , nghĩa là **đứng trước các đối số của nó** .

### Trung tố - Infix

Hãy xem biểu thức sau:

```haskell
1 + 2
```

`+` thực sự là một hàm! Và được viết ở **dạng trung tố** , nghĩa là **đứng giữa các đối số của nó** .

Các hàm được thiết kế để sử dụng ở dạng trung tố được gọi là **toán tử - operators** .

Và làm cách nào để biết một hàm là trung tố hay tiền tố? Chà ...

Các hàm được định nghĩa **chỉ bằng các ký hiệu** sẽ được tự động xem như **hàm trung tố** , nếu không thì chúng là các hàm tiền tố.

Nhưng bạn vẫn có thể sử dụng hàm trung tố như hàm tiền tố và ngược lại.

### Cách gọi hàm trung tố như tiền tố và ngược lại

Chúng ta sử dụng dấu ngoặc đơn xung quanh hàm trung tố để sử dụng nó như hàm tiền tố:

```haskell
(+) 1 2
```

Để kiểm tra kiểu của hàm infix, chúng ta cũng phải đặt tên của nó trong dấu ngoặc đơn:

```haskell
:t (+)
```

<div class="alert alert-block alert-info"> Chắc hẳn bạn đã nhận thấy chữ ký kiểu của <code>+</code> trông khác với những chữ ký trước đó. Bởi vì nó sử dụng các kiểu đa hình và lớp kiểu. Chúng ta sẽ tìm hiểu về các kiểu đa hình trong bài này và về các lớp kiểu trong các bài sau. Còn bây giờ, đừng quá bận tâm về nó.</div>

Chúng ta sử dụng các dấu backtick <code>`</code> bao quanh hàm tiền tố để sử dụng nó như dạng trung tố:

```haskell
4 `prod` 5
```

## Các kiểu dữ liệu phổ biến

### Các kiểu số nguyên: `Int` và `Integer`

- `Integer` là kiểu có độ chính xác tùy ý: Nó sẽ chứa bất kỳ số nguyên nào - dù lớn đến đâu - tùy thuộc vào giới hạn bộ nhớ máy của bạn.

Điều này có nghĩa là bạn sẽ không bao giờ bị tràn số học, nhưng nó cũng đồng nghĩa với việc số của bạn sẽ tương đối chậm.

- Mặt khác, các giá trị `Int` giới hạn trong phạm vi `±2^63` *(đối với CPU 64 bit)*.

Điều này giới hạn giá trị mà `Int` có thể biểu diễn, nhưng giúp nó có hiệu suất tốt hơn.

Hãy xem điều này trong thực tế:

```haskell
2^62 :: Int -- All good
```

```haskell
2^64 :: Int -- Oh no!
```

```haskell
2^127 :: Integer -- All good again
```

Nhưng còn số thực? Số có phần thập phân thì sao? Chúng ta có `Float` và `Double` .

### Các kiểu số thực dấu phẩy động: `Float` và `Double`

`Float` là kiểu số thực dấu phẩy động với độ chính xác đơn (32 bit), trong khi `Double` là kiểu số thực dấu phẩy động với độ chính xác gấp đôi (64 bit).

Hãy xem điều gì sẽ xảy ra nếu chúng ta muốn hiển thị 20 chữ số đầu tiên của pi (π) ở cả hai kiểu:

```haskell
3.14159265358979323846 :: Float

3.14159265358979323846 :: Double
```

Có thể nói rằng `Double` chính xác hơn `Float` .

Về mặt lý thuyết, lý do khi nào nên sử dụng cái này hay cái kia hơi giống với trường hợp `Int` và `Integer` . `Double` có độ chính xác gấp đôi nhưng tốn nhiều bộ nhớ hơn vì nó sử dụng số bit nhiều gấp đôi để biểu thị số.

NHƯNG!

Khuyến nghị dựa trên thực tế:

- **Ngay cả khi bạn không đặc biệt quan tâm đến độ chính xác của giá trị, hãy sử dụng `Double` .** Máy tính hiện đại hiếm khi gặp bất lợi về tốc độ và với `Double` , bạn sẽ ít có khả năng tự bắn vào chân mình với các lỗi làm tròn.

- Nếu bạn ở trong bối cảnh nơi **các số liệu chính xác là quan trọng** (ví dụ: tài chính và kế toán), một ý tưởng tốt là **sử dụng các kiểu dữ liệu `Rational` hoặc `Decimal`**. Vì chúng tránh hoàn toàn các lỗi làm tròn. Chúng ta sẽ đề cập đến chúng trong các bài học sau.

### Kiểu boolean `Bool`

Kiểu boolean `Bool` chỉ chứa hai giá trị: `True` và `False` .

Các số, ký tự và chuỗi có thể được so sánh bằng cách sử dụng **các toán tử so sánh** thông thường để tạo ra giá trị `Bool` : `==`, `/=`, `<=`, `>=`, `<`, `>`

```haskell

5 /= 0 -- True

3 >= 0 -- True

7.2 < 6.1 -- False

pi > 3.14 -- True
```

Ngoài ra còn có các toán tử `&&` ( **AND** ) và `||` ( **OR** ) cho phép chúng ta kết hợp các giá trị:

- Toán tử `&&` (AND) trả về `True` nếu cả hai giá trị boolean bên trái và bên phải của nó đều là `True` .
- `||` Toán tử (OR) trả về `True` nếu một trong hai giá trị đó là `True` .

```haskell
:t (&&)
:t (||)

True && False
True || False
```

### Kiểu ký tự `Char`

`Char` là kiểu chúng ta sử dụng để thể hiện một ký tự *Unicode* .

<div class="alert alert-block alert-info">
<p> Tiêu chuẩn Unicode (Unicode) là một bộ quy tắc thực thi cách xử lý và thể hiện văn bản. Nó cần thiết vì máy tính suy nghĩ bằng các số (số 1 và số 0) và chúng ta phải thống nhất xem số nào đại diện cho ký tự nào.</p>
<p>Thực ra nó còn phức tạp hơn một chút (xem thêm: <a href="https://en.wikipedia.org/wiki/Character_encoding">Character encoding</a>). Nhưng với mục đích của chúng ta, chỉ cần biết rằng có thể biểu diễn hầu hết mọi ký tự cần thiết bằng cách sử dụng Unicode. Bao gồm các chữ cái, số và hơn 140.000 ký hiệu khác.</p>
</div>

Chúng ta viết các giá trị kiểu Char (ký tự Unicode) giữa các dấu nháy đơn. Như thế này:

```haskell
'a'
'@'
'7'
```

Lưu ý rằng nếu bạn viết một số được bao quanh bởi dấu nháy đơn (như trong biểu thức cuối cùng), Haskell sẽ không coi số đó là một số. Nó sẽ đối xử với nó như bất kỳ ký tự nào khác. Vì vậy, bạn không thể làm toán với `'7'` (có dấu nháy đơn), nhưng có thể với `7` (không có dấu nháy đơn).

<div class="alert alert-block alert-warning"> Quan trọng: Bạn chỉ có thể viết một ký tự mỗi lần! Một thứ như <code>'hi'</code> không phải là một <code>Char</code> hợp lệ!</div>

Vậy làm sao để viết được câu đầy đủ? Tôi sẽ nói với bạn. Nhưng trước hết chúng ta phải tìm hiểu về danh sách.

### Kiểu danh sách - List

Trong Haskell, **danh sách là một cấu trúc dữ liệu đồng nhất** .

Nói đơn giản hơn, chúng là các danh sách lưu trữ các phần tử có cùng kiểu. Vì vậy, chúng ta có thể có danh sách `Int` hoặc danh sách `Char` nhưng không thể có danh sách hỗn hợp.

- Danh sách được biểu thị bằng dấu ngoặc vuông `[1,5,3,-4,0]` và các giá trị trong danh sách được **phân tách bằng dấu phẩy** .

- Kiểu của danh sách được biểu thị bằng kiểu của phần tử chứa trong đó, được bao quanh bởi dấu ngoặc vuông. Danh sách kiểu `[Int]` chứa các số kiểu `Int` . Danh sách kiểu `[Char]` chứa các phần tử thuộc kiểu `Char` .

```haskell
:t ['a', 'b', 'c', 'd']

:t [True,False, 3 > 2, 'a' == 'b']
```

### Kiểu chuỗi <code>String</code>

**Chuỗi đại diện cho danh sách các ký tự.** Bạn có thể sử dụng kiểu `String` để viết tin nhắn, giá trị chữ và số, ký hiệu, v.v. Không giống như `Char`, `String` được bao quanh bằng **dấu nháy kép** như thế này:

```haskell
"Hellooooooo!"
```

Điều đó có nghĩa là hai giá trị dưới đây là giống nhau!:

```haskell
['H','i','!'] == "Hi!"
```

Như vậy `String` và `[Char]` là cùng một kiểu! Cụ thể hơn, `String` là cú pháp dễ đọc - syntactic sugar (cú pháp được thiết kế để làm cho mọi thứ dễ đọc hoặc diễn đạt dễ dàng hơn) của `[Char]` ! Vì vậy, bạn có thể sử dụng chúng thay thế cho nhau!

Thứ mà bạn không thể sử dụng thay thế cho nhau trong Haskell là dấu nháy đơn và dấu nháy kép. `String` (được viết trong dấu nháy kép) là danh sách các phần tử `Char` (được viết trong dấu nháy đơn). Chúng không giống nhau!

```haskell
:t "A"
:t 'A'
```

Mọi lập trình viên đều biết rằng danh sách cực kỳ hữu ích. Nhưng nếu bạn muốn gộp các giá trị thuộc các kiểu khác nhau thì sao? Đó là khi bộ dữ liệu trở nên hữu ích!

### Bộ dữ liệu - Tuple

Bộ dữ liệu - Tuple là cấu trúc được sử dụng để lưu trữ **các phần tử không đồng nhất** dưới dạng một giá trị.

Chúng ta biểu diễn các tuple bắt đầu bằng dấu mở ngoặc đơn, rồi viết tất cả các phần tử phân cách với nhau bởi dấu phẩy và kết thúc bằng dấu đóng ngoặc đơn. Đây là một ví dụ về một tuple có 3 phần tử:

```haskell
('a', 3, True)
```

Nghe có vẻ giống danh sách nhưng có hai điểm khác biệt chính:

- **Tuple có thể lưu trữ các phần tử thuộc nhiều kiểu khác nhau:** Bạn có thể thấy trong ví dụ trên, tuple có thể lưu trữ các phần tử thuộc các kiểu khác nhau, trong khi danh sách thì không.

- **Tuple có kích thước cố định:** Bạn có thể tăng kích thước danh sách bằng cách nối hoặc các phương thức khác, nhưng bạn không thể tăng hoặc giảm kích thước của tuple. Khi bạn định nghĩa một tuple có N phần tử, nó sẽ luôn có N phần tử.

**Và những khác biệt chính đó được phản ánh trong kiểu của tuple.**

Kiểu của tuple phụ thuộc vào:

- Kiểu của các phần tử của nó.
- Thứ tự của các phần tử.
- Số lượng của các phần tử.

Ví dụ:

```haskell
:t ('a', True)

:t (True, 'a')

:t (True, 'a', 'b')

:t (True)
```

Như bạn có thể thấy, `('a', True) :: (Char, Bool)` , `(True, 'a') :: (Bool, Char)` , và `('a', True, True) :: (Char, Bool, Bool)`, tất cả đều có kiểu khác nhau. Với trình biên dịch, ba tuple đó là khác nhau, cũng giống như `Float` với `Char` .

Bạn có nhận thấy rằng nếu bạn cố gắng tạo một tuple một phần tử, GHCi chỉ trả về phần tử đó không? (biểu thức cuối cùng của khối mã trên). Đó là vì không có tuple một phần tử! Việc có một tuple một phần tử sẽ không mang lại thêm giá trị gì. Vì vậy, trong trường hợp này, Haskell bỏ qua tuple và chỉ đánh giá phần tử.

## Giá trị đa hình và biến kiểu

Điều tuyệt vời về các kiểu là chúng bảo vệ chúng ta khỏi chính mình! Nếu chúng ta nói rằng một hàm nhận đầu vào thuộc kiểu `[Char]` , Haskell sẽ kiểm tra xem chúng ta có đáp ứng yêu cầu đó mỗi khi sử dụng hàm đó hay không. Nếu chúng ta truyền vào kiểu `Double`, trình biên dịch sẽ yêu cầu chúng ta sửa lỗi!

Nhưng giờ có một vấn đề! Hãy tưởng tượng rằng chúng ta tạo hàm `prod` :

```haskell
prod :: Int -> Int -> Int
prod x y = x * y
```

Nó hoạt động hoàn hảo với các giá trị thuộc kiểu `Int`. Nhưng nếu chúng ta cần dùng nó cho các giá trị thuộc kiểu `Double` thì sao? Chúng ta biết nó vẫn sẽ hoạt động vì chúng đều là các số và công thức sẽ cho ra kết quả chính xác.

Chúng ta *có thể* tạo một hàm mới thực hiện việc tương tự nhưng với một kiểu khác:

```haskell
prodForDubles :: Double -> Double -> Double
prodForDoubles x y = x * y
```

Về mặt kỹ thuật, cách này hoạt động. Nhưng còn kiểu `Float` và `Integer` thì sao? Nếu cứ phải tạo các hàm trùng lặp cho từng trường hợp, cách này sẽ nhanh chóng trở nên không hiệu quả!

**Kiểu dữ liệu đa hình xuất hiện để giải cứu!**

Đa hình có nghĩa là một cái gì đó có nhiều hình thức. Và **giá trị đa hình là giá trị có thể có nhiều kiểu** . (Ví dụ `4` có thể là `Int` , `Integer` , `Float` ,...)

Ví dụ, hãy tưởng tượng rằng chúng ta muốn tạo một hàm nhận vào một tuple có hai giá trị (còn gọi là cặp - pair) và trả về giá trị đầu tiên. Như thế này:

```haskell
first (x,y) = x
```

Nó nên có kiểu gì? Tôi không quan tâm lắm đến kiểu của các phần tử vì tôi không làm gì với chúng! Tôi không làm toán số học, xử lý văn bản hay bất cứ thứ gì! Mà tôi chỉ muốn lấy ra phần tử đầu tiên, thế là xong!

Trong những trường hợp như này, chúng ta khai báo chữ ký có các biến kiểu!

```haskell
first :: (a, b) -> a
first (x,y) = x

first ('a', "hi!")
```

Chữ ký đó được đọc là: "Hàm `first` nhận vào một pair có kiểu `(a, b)` và trả về một giá trị kiểu `a` ."

<div class="alert alert-block alert-warning"> <b>Quan trọng:</b> Các kiểu cụ thể (ví dụ: <code>Char</code> , <code>Bool</code> , <code>Int</code> ) bắt đầu bằng chữ in hoa. Nhưng các kiểu đa hình bắt đầu bằng chữ thường. Chúng ta có thể sử dụng tên dài hơn cho các kiểu đa hình, nhưng thông thường là sử dụng các chữ cái đơn (ví dụ: <code>a</code> , <code>b</code> , <code>c</code> ).</div>

Hàm " `first` " mà chúng ta vừa tạo thực ra có sẵn trong Haskell, với tên là `fst` ! Và nó đi kèm với bạn đồng hành của nó: `snd` !:

```haskell
:t fst
:t snd

fst (1,2)
snd (1,2)
```

`a` và `b` là các biến kiểu, nghĩa là chúng có thể thuộc bất kỳ kiểu nào. Và bất kể chúng là gì, giá trị trả về bởi `first` luôn có cùng kiểu với phần tử đầu tiên của pair (vì cả hai đều thuộc kiểu `a` ).

Bằng cách sử dụng các biến kiểu, chúng ta có thể sử dụng hàm `first` với các pair thuộc bất kỳ kiểu nào (giá trị đa hình)!

Lưu ý rằng cả `a` và `b` đều CÓ THỂ thuộc bất kỳ kiểu nào VÀ chúng là các kiểu khác nhau. Nhưng KHÔNG NHẤT THIẾT phải như vậy. Bạn có thể sử dụng `first` trên một tuple có cùng kiểu: `('a','b') :: (Char, Char)` .

Một ví dụ khác về hàm đa hình là `head` và `tail` .

Bạn có thể sử dụng `head` để lấy phần tử đầu tiên của danh sách và `tail` để lấy tất cả các phần tử của danh sách *ngoại trừ* phần tử đầu tiên.

```haskell
list = [1,2,3,4]
list

:t head
head list

:t tail
tail list
```

Chúng ta không quan tâm đến các kiểu cụ thể. Chúng ta chỉ đang trích xuất một phần tử. Vì vậy, tham số là một danh sách đa hình (danh sách của bất kỳ kiểu nào, hãy gọi nó là `[a]` ). Và kết quả phải là một phần tử cùng kiểu với các phần tử trong danh sách. Đó là lý do tại sao nó phải là `a` .

Bây giờ chúng ta đã quen thuộc với tất cả các kiểu này, hãy cùng vui vẻ một chút với danh sách! (Chúng ta sẽ để dành niềm vui với các tuple sau khi học về Pattern Matching - Khớp mẫu. Sẽ rất thú vị đấy.)

## Vui vẻ với danh sách!

Mỗi phần tử đều có chỉ mục được xác định bởi vị trí của nó trong danh sách - bắt đầu từ 0 (không).

Chúng ta dùng toán tử `!!` để truy cập một phần tử cụ thể bên trong danh sách bằng cách sử dụng chỉ mục của nó:

```haskell
:t (!!)
"abc" !! 1
[12,13,16,18] !! 3
```

Tuple không có chỉ mục, vì vậy người ta không thể dễ dàng trích xuất các phần tử của các tuple theo cách này. Tuy nhiên, chúng ta có thể sử dụng `fst` và `snd` cho các pair và khớp mẫu cho các tuple dài hơn. (Xem bài học về khớp mẫu để biết cách thực hiện.)

Danh sách có thể được xác định bởi một phạm vi:

```haskell
[3..22]
```

Và chúng ta cũng có thể chỉ định bước nhảy giữa các phần tử của phạm vi:

```haskell
[3,5..22]
['a','c'..'z']
```

Kết quả của biểu thức đầu tiên sẽ chứa tất cả các phần tử bắt đầu từ `3` với bước nhảy `2 = 5 - 3` và không vượt quá `22` (nếu phần tử cuối cùng không phù hợp với mẫu bước nhảy đã định, nó sẽ bị loại khỏi kết quả) .

Kết quả của biểu thức thứ hai sẽ là <code>"acegikmoqsuwy"</code>.

Điều quan trọng cần lưu ý là bạn chỉ có thể chỉ định kích thước của một bước nhảy!

Nếu bước nhảy là âm thì các phần tử sẽ được liệt kê theo thứ tự giảm dần:

```haskell
[17,14..3]
```

Bạn cũng có thể sử dụng phạm vi để tạo danh sách vô hạn bằng cách không chỉ định giới hạn trên.

- `[1..]` là danh sách vô hạn `[1,2,3,4,5,...]`.

- `[1,3..]` là danh sách vô hạn `[1,3,5,7,9,...]`.

Bây giờ, nếu chúng ta chỉ tính toán danh sách, chương trình sẽ chạy mãi mãi (hoặc cho đến khi nó đổ vỡ). Vì vậy, danh sách vô hạn thường được sử dụng như một phần của biểu thức.

Chúng ta cũng có hàm `take` trả về một danh sách chứa `n` phần tử đầu tiên trong danh sách (có thể vô hạn) `l` .

```haskell
:t take

take 3 ['x'..]

take 20 [1,3..]

take 7 [5,3..]
```

Chúng ta sử dụng toán tử *cons* (được biểu thị bằng dấu hai chấm `:` ) để thêm một phần tử vào trước danh sách:

```haskell
:t (:)
2 : [3,4,5]
```

Và chúng ta sử dụng toán tử **nối** `++` để ghép hai danh sách lại với nhau:

```haskell
:t (++)
[1,3,7,9] ++ [3,3,1]
```

Lưu ý rằng `++` là hàm nhận hai danh sách và `:` là hàm nhận một phần tử và một danh sách.

**Cảnh báo:** Việc sử dụng toán tử `++` trên các danh sách dài (ngay cả khi bạn thêm một danh sách đơn vào một danh sách, chẳng hạn: `[1,2,3] ++ [4]` ), buộc Haskell phải **duyệt qua toàn bộ danh sách** phía bên trái của `++` . Do đó, việc đặt thứ gì đó vào cuối một danh sách dài 50 triệu phần tử sẽ mất một khoảng thời gian kha khá! Tuy nhiên, việc đặt một thứ gì đó vào đầu danh sách bằng toán tử cons `:` sẽ diễn ra ngay lập tức!

Trong số nhiều hàm hữu ích được định nghĩa cho danh sách, chúng ta hãy điểm qua một vài hàm sau:

- `length` nhận một danh sách và trả về độ dài của nó;

- `null` kiểm tra xem danh sách có rỗng không;

- `sum` nhận một danh sách các số và trả về tổng của chúng;

- `elem` nhận một phần tử `x` và một danh sách `l` chứa các phần tử cùng kiểu và kiểm tra xem `x` có phải là phần tử của danh sách `l` hay không.

```haskell
length [2,4,5,6,7]

null [2]

sum [-1,0,1,6,-5,-1]

5 `elem` [6,3,5,7,5]
```

Đó là mọi thứ về danh sách cho đến hiện tại. Chúng ta sẽ tiếp tục học thêm về chúng trong khóa học!

### Nối và ngắt văn bản

Có những tình huống khi bạn muốn thực hiện các thao tác liên quan đến văn bản với danh sách của mình. Haskell có các hàm chuyên dụng cho việc đó.

Ví dụ:

- `words :: String -> [String]` chia `String` thành một danh sách các từ được phân cách bằng khoảng trắng.

- `unwords :: [String] -> String` là một thao tác trái ngược với <code>words</code>. Nó nối các từ lại với nhau dùng khoảng trắng để phân tách.

- `lines :: String -> [String]` chia đối số thành một danh sách các dòng, với các ký tự xuống dòng ( `\n` ) đóng vai trò là dấu phân tách.

- `unlines :: [String] -> String` tạo một `String` từ một danh sách các chuỗi, chèn thêm các ký tự xuống dòng ( `\n` ) giữa các chuỗi gốc.

```haskell
words "To be or not to be?"

lines "How are you doing? \n I'm fine, how about you?"
```
