```haskell
:opt no-lint
```

# Đệ quy và các hàm Fold

## Nội dung

- Tại sao là đệ quy?
- Suy nghĩ kiểu đệ quy
    - `sum` và `product`
- Các bước để tạo hàm đệ quy của riêng bạn
- Ví dụ về đệ quy
    - `and` , `length` , `reverse` , `drop` , `take` , `map` , `filter`
- Trích rút mẫu cho `foldr`
- Hàm `foldl`
- Hàm `foldl'`
- Khi nào sử dụng `foldr` , `foldl` và `foldl'`

## Tại sao là đệ quy?

Một trong những tính năng cơ bản cần thiết trong bất kỳ ngôn ngữ lập trình nào là sự lặp lại. Ví dụ:

- Bạn có một danh sách các đối tượng và muốn làm gì đó với tất cả chúng. Từng cái một.
- Bạn muốn thực hiện một số phép tính 5 lần với các giá trị khác nhau.
- Vân vân.

Trong các ngôn ngữ lập trình mệnh lệnh, các tác vụ lặp đi lặp lại này được xử lý bằng cách sử dụng các vòng lặp. Ví dụ: trong JavaScript, bạn có thể có:

```javascript
for (i = 0; i < 5; i = i + 1) {
    // Do something
}

let i = 0;
while (i < 5) {
  // Do something
  i = i + 1;
}
```

Tuy nhiên, nếu cố gắng tạo ra thứ gì đó như thế này trong Haskell, chúng ta sẽ gặp vấn đề lớn. Đó là biến `i` .

Như chúng ta đã đề cập trong bài 1, Haskell là một ngôn ngữ hàm thuần túy. Nhưng hai khối lệnh trên phụ thuộc vào việc thay đổi `i` trong mỗi lần lặp. Điều đó có nghĩa là chúng có hiệu ứng phụ là cập nhật trạng thái toàn cục khi chương trình chạy.

Vì vậy, trong Haskell, chúng ta không có những hàm lặp tích hợp này. Thay vào đó, chúng ta có đệ quy!

Bạn tự hỏi: đệ quy tốt hơn vòng lặp như thế nào? Dưới đây là một vài lý do:

Lý do tại sao đệ quy hữu ích:

- Mọi thứ bạn có thể làm với vòng lặp, bạn có thể thực hiện bằng đệ quy. Và trên hết, thậm chí có những chương trình mà bạn có thể định nghĩa bằng đệ quy mà không thể viết bằng vòng lặp `for` .
- Nhiều hàm (nếu không phải là hầu hết) có thể được định nghĩa một cách tự nhiên bằng cách sử dụng đệ quy. Điều này có nghĩa là cách bạn suy nghĩ một cách trừu tượng về hàm và cách bạn viết nó bằng đệ quy rất tương đồng.
- Một số hàm sẽ rõ ràng và ngắn gọn hơn nếu được định nghĩa bằng đệ quy.
- Bạn có thể sử dụng quy nạp để lập luận toán học và chứng minh tính chất của các hàm được định nghĩa bằng đệ quy. (Chuyên sâu hơn, nhưng cực kỳ mạnh mẽ.)

Bây giờ bạn đã biết mình sắp học được một khái niệm khá mạnh mẽ, hãy cùng tìm hiểu nhé!

## Suy nghĩ kiểu đệ quy

Đệ quy xảy ra khi một thứ gì đó được định nghĩa dựa trên chính nó. Vì vậy, một hàm đệ quy là một hàm được định nghĩa dựa trên chính nó.

Nó đó. Khái niệm này thực sự đơn giản. Việc thực hiện là điều gây ra nhiều rắc rối nhất. Vì vậy, chúng ta sẽ bắt đầu bằng cách định nghĩa một hàm bằng cả vòng lặp `for` (sử dụng Python) và đệ quy (sử dụng Haskell) để làm nổi bật sự khác biệt trong cách suy nghĩ về vấn đề.

Giả sử chúng ta muốn tính tổng của một danh sách các số.

Cả Python và Haskell đều có hàm `sum` để làm điều này rồi. Nhưng lần này, chúng ta sẽ tạo nó từ đầu. Trong các ngôn ngữ mệnh lệnh, bạn sẽ viết một cái gì đó như thế này:

```python
def sum(list):
    total = 0
    for i in list:
        total = total + i
    return total
```

Ở đây, bạn đang mô tả từng bước những gì chương trình sẽ làm:

1. Chúng ta tạo một hàm có tên `sum` nhận vào một `list` .
2. Sau đó, chúng ta tạo một biến có tên là `total` với giá trị ban đầu là `0` .
3. Sau đó, đối với mỗi phần tử trong danh sách, chúng ta lấy `total` , cộng thêm phần tử vào nó và gán đè giá trị mới này cho `total`.
4. Sau khi vòng lặp kết thúc, hàm trả về biến `total` .

Như bạn có thể thấy, trong các ngôn ngữ mệnh lệnh, chúng ta sử dụng một chuỗi câu lệnh để xác định CÁCH đạt được mục tiêu. Trong trường hợp này là tổng của các phần tử trong danh sách.

Để dễ dàng viết các hàm đệ quy, bạn phải loại bỏ lối suy nghĩ đó và áp dụng lập trình khai báo. Nơi bạn mô tả bản chất sự vật LÀ gì thay vì cách để đạt được chúng từng bước.

Bây giờ, hãy định nghĩa hàm tương tự trong Haskell.

Như mọi khi, điều đầu tiên chúng ta cần làm là viết chữ ký kiểu:

```haskell
sum :: [Int] -> Int
```

Như vậy, chúng ta biết nó nhận một danh sách các số nguyên và trả về một số nguyên.

Bây giờ, dựa trên BẢN CHẤT của hàm: Hàm này nhận một danh sách các số và trả về tổng của chúng, bước tiếp theo là tìm các trường hợp biên (edge case).

Chúng ta lấy một danh sách làm đầu vào. Điều gì xảy ra nếu danh sách rỗng chẳng hạn? Chà, trong trường hợp đó, chúng ta biết rằng tổng của một danh sách rỗng LÀ `0` . Vì vậy, chúng ta có thể bắt đầu bằng cách xác định rằng:

```haskell
sum :: [Int] -> Int
sum [] = 0
```

Ngoài ra, còn trường hợp có các phần tử trong danh sách:

```haskell
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) =
```

Nếu chúng ta nghĩ về BẢN CHẤT của hàm `sum` theo định nghĩa thứ hai, thì đó là một hàm nhận vào một danh sách `Int` không rỗng và cộng chúng lại. Điều này cũng giống như việc cộng `x` (phần tử đầu tiên) vào kết quả của việc cộng tất cả các `Int` trong `xs` . Vì vậy, chúng ta có thể làm điều gì đó như sau:

```haskell
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + ...
```

Và bây giờ, chúng ta cần tìm tổng của tất cả các phần tử trong `xs` . Nhưng đợi một chút... chúng ta đã có sẵn hàm để làm điều đó rồi! Nó chính là hàm chúng ta đang định nghĩa! Vì vậy, chúng ta chỉ cần sử dụng nó!:

```haskell
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs
```

Và đây! Chúng ta đã triển khai hàm đệ quy đầu tiên của mình! Tại sao? Bởi vì chúng ta đã định nghĩa hàm `sum` bằng chính nó!

Hãy xem điều gì xảy ra khi chúng ta sử dụng hàm này. Ví dụ: hãy tính tổng của một danh sách chứa tất cả các số nguyên từ `1` đến `5` :

```haskell
sum [1,2,3,4,5] = 1 + sum [2,3,4,5]
                = 1 + 2 + sum [3,4,5]
                = 1 + 2 + 3 + sum [4,5]
                = 1 + 2 + 3 + 4 + sum [5]
                = 1 + 2 + 3 + 4 + 5 + sum []
                = 1 + 2 + 3 + 4 + 5 + 0
                = 15
```

Và đó là cách Haskell tính toán hàm của chúng ta.

Lưu ý rằng trường hợp cơ sở (base case) là trường hợp cho phép chúng ta dừng đệ quy và có kết quả. Nếu chúng ta định nghĩa hàm đệ quy mà không có trường hợp cơ sở, nó sẽ bị lỗi hoặc chạy mãi mãi.

Vì vậy, tóm lại:

Với vòng lặp, bạn thay đổi ngữ cảnh với một giá trị tích lũy (accumulator) có thể biến đổi, đóng gói thành các bước để xác định CÁCH đạt được mục tiêu.

Với đệ quy, bạn bao gói hàm với chính nó, tạo ra một ngữ cảnh mới với biến đổi mong muốn. Và hàm đó, lần lượt, tự gọi lại chính nó, thiết lập ngữ cảnh của nó và cứ như vậy tiếp tục.

Bây giờ, mặc dù đây là hướng dẫn đầy đủ về cách tạo hàm đệ quy  `sum` , nhưng những lập luận có vẻ hơi quá cụ thể để áp dụng cho các hàm khác.

Để giúp bạn dễ dàng tạo các hàm đệ quy của riêng mình, chúng tôi sẽ liệt kê từng bước cơ bản để bạn có thể áp dụng cho mọi trường hợp. Hãy cùng xem!

## Các bước để tạo hàm đệ quy của riêng bạn

Tôi đã chuẩn bị một phiên bản sửa đổi một chút của các bước được tạo ra bởi Tiến sĩ Graham Hutton. Nhà nghiên cứu, giáo viên và thành viên hội đồng quản trị nổi tiếng của Quỹ Haskell. Vì vậy... bạn biết đấy... đây là các bước thực sự:

1. Viết xuống kiểu dữ liệu: Điều này sẽ giúp bạn khi định nghĩa hàm sau đó. (Bạn nên luôn khai báo kiểu trước, ngay cả khi bạn không định nghĩa một hàm đệ quy.)
2. Liệt kê các trường hợp có thể có dựa trên đầu vào của hàm. (Bắt đầu với các trường hợp "tiêu chuẩn" và thay đổi hoặc tinh chỉnh chúng nếu cần.)
3. Trong tất cả các trường hợp đã được khởi tạo trước đó, xác định những trường hợp nào đơn giản nhất và định nghĩa chúng. (chúng thường là các trường hợp cơ sở (trường hợp biên))
4. Hãy suy nghĩ về những gì bạn có sẵn (tham số, hàm, toán tử, các giá trị khác, các toán tử cho kiểu đó, v.v.).
5. Định nghĩa các trường hợp còn lại.
6. Xem xét lại hàm. Định nghĩa có thể được đơn giản hóa không? Chữ ký có thể được tổng quát hóa không? (chúng ta sẽ xem cách thực hiện điều đó trong các bài học sau) Nó có thực hiện được những gì bạn dự định không?

Không phải lúc nào bạn cũng phải trải qua các bước này. Khi cảm thấy thoải mái hơn, bạn có thể bỏ qua một vài thao tác hoặc thậm chí viết hàm ngay lập tức.

Nói chung, trường hợp cơ sở (biên) thường là trường hợp "identity". Một trường hợp không sửa đổi kết quả mà chỉ dừng đệ quy. Ở đây chúng ta có một vài ví dụ:

Hai mẫu tiêu chuẩn phổ biến:

- Đối với các hàm đệ quy lấy số không âm làm đầu vào, bạn thường (không phải luôn luôn) có trường hợp cơ sở (biên) là `0` hoặc `1` (tùy thuộc vào thao tác) và trường hợp đệ quy là `n` .
- Đối với các hàm đệ quy lấy danh sách làm đầu vào, bạn thường (không phải luôn luôn) có trường hợp cơ sở (biên) là `[]` (danh sách rỗng) và trường hợp đệ quy là `(x:xs)` (danh sách không rỗng).

Vì vậy, nếu chúng ta muốn sửa hàm `sum` để tính tích của các phần tử trong danh sách và chúng ta chỉ thay đổi trường hợp đệ quy như sau:

```haskell
product :: [Int] -> Int
product [] = 0
product (x:xs) = x * product xs -- Only changed + to *
```

Chúng ta gặp phải vấn đề là hàm luôn trả về `0` . Bởi vì tất cả các phần tử của danh sách sẽ được nhân với `0` khi kết thúc đệ quy do trường hợp cơ sở!

Vì vậy, thay vào đó, cách chính xác để xác định trường hợp cơ sở cho `product` là cung cấp "identity" cho hàm ( `*` ), là `1` :

```haskell
product :: [Int] -> Int
product [] = 1
product (x:xs) = x * product xs
```

Và đó. Chúng ta đã định nghĩa hàm đệ quy thứ hai.

Thực hành là điều sẽ mang lại cho bạn trực giác cần thiết để nhanh chóng định nghĩa được các hàm đệ quy. Vì vậy, hãy thực hành thật nhiều để có được trực giác đó! 💪

## Ví dụ về đệ quy

Lưu ý: Tôi đã thêm `'` vào tất cả tên hàm vì tất cả các hàm này đều đã tồn tại trong Haskell.

#### `and'` : Hàm trả về `True` khi và chỉ khi **tất cả** các phần tử của danh sách là `True` .

Như vậy, nó nhận một danh sách các boolean và trả về một boolean. Kiểu của nó:

```haskell
and' :: [Bool] -> Bool
```

Bây giờ, vì hàm nhận vào một danh sách nên chúng ta sẽ xác định các trường hợp tiêu chuẩn cho danh sách:

```haskell
and' :: [Bool] -> Bool
and' []     =
and' (x:xs) =
```

Trường hợp cơ sở có vẻ không quá rõ ràng. Dĩ nhiên, chỉ có hai giá trị để chọn vì đó là `Bool` . Nhưng chọn cái nào? Vậy nên, chúng ta sẽ bắt đầu với trường hợp đệ quy.

Bây giờ, hãy nghĩ về những gì chúng ta có sẵn. Bởi vì chúng ta đang xử lý `Bool` , nên chúng ta có quyền truy cập vào tất cả các phép toán boolean. Và có một cái thực hiện được những gì chúng ta cần nhưng chỉ giữa hai giá trị. Toán tử `&&` (and).

Vì vậy, phần tử đầu tiên kết hợp sử dụng `&&` với kết quả xử lý phần còn lại của danh sách cho chúng ta kết quả mong muốn:

```haskell
and' :: [Bool] -> Bool
and' []     =
and' (x:xs) = x && ...
```

Và bây giờ chúng ta phải trả về `True` khi và chỉ khi **tất cả** các phần tử của danh sách `xs` là `True` . Có nghĩa là chúng ta cần hàm tương tự mà chúng ta đang định nghĩa ngay bây giờ. Vì vậy, chúng ta áp dụng nó cho `xs` :

```haskell
and' :: [Bool] -> Bool
and' []     =
and' (x:xs) = x && and' xs
```

Và bây giờ, trường hợp cơ sở đã rõ ràng! Nếu chúng ta sử dụng `False` , thì bất kể chúng ta xử lý danh sách nào, sẽ luôn nhận được `False` vì `&& False` luôn bằng `False` .

Nhưng nếu chúng ta sử dụng `True` , chúng ta sẽ không làm thay đổi kết quả! Bởi vì kết quả của `&& True` phụ thuộc vào phía bên trái bị thiếu. Nếu có một phần tử không `True` trong danh sách, nó sẽ trả về `False` cho đến hết. Ngược lại, nó sẽ cho chúng ta `True` !

Một cách khác để tìm ra điều này là nhận ra rằng `True` là "identity" của `&&` :

```haskell
and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and xs

and' [True, False, True]
and' [2 < 3, 4 == 4]
```

```
False

True
```

#### `length'` : Hàm cung cấp cho bạn độ dài của danh sách

Để tính độ dài của danh sách, chúng ta phải lấy một danh sách và trả về một số nguyên. Và bởi vì, về nguyên tắc, chúng ta sẽ không thao tác trên các thành phần của danh sách, nên chúng ta có thể sử dụng kiểu đa hình như thế này:

```haskell
length' :: [a] -> Int
```

Bây giờ, vì hàm nhận vào một danh sách nên chúng ta sẽ xác định các trường hợp tiêu chuẩn cho danh sách:

```haskell
length' :: [a] -> Int
length' []     =
length' (x:xs) =
```

Bây giờ, hãy nhìn vào trường hợp đơn giản nhất, chúng ta có thể xác định rằng độ dài của danh sách rỗng, tất nhiên là `0` phần tử. Vì vậy, chúng ta thêm nó vào:

```haskell
length' :: [a] -> Int
length' []     = 0
length' (x:xs) =
```

Và bây giờ, chúng ta có thể tính độ dài của danh sách nếu chúng ta cộng thêm `1` ứng với mỗi thành phần của danh sách, phải không? Và bởi vì chúng ta có phần tử đầu tiên ( `x` ) được chọn ra bằng khớp mẫu, nên chúng ta có thể thêm `1` cho nó và tính toán đệ quy độ dài của phần còn lại của danh sách ( `xs` ):

```haskell
length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs
```

Đó có thể là hàm cuối cùng. Nhưng vì chúng ta không thực sự sử dụng `x` nên có thể bỏ qua nó trong mẫu của mình:

```haskell
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

length' [1,2,3,4,5]
length' ['a'..'z']
```

```
5

26
```

Và đó là định nghĩa cuối cùng của chúng ta.

##### `reverse'` : Hàm đảo ngược danh sách.

Để đảo ngược danh sách, chúng ta lấy danh sách các phần tử và trả về danh sách các phần tử. Và bởi vì, về nguyên tắc, chúng ta sẽ không thao tác trên các phần tử của danh sách, nên chúng ta có thể sử dụng kiểu đa hình như thế này:

```haskell
reverse' :: [a] -> [a]
```

Bây giờ, vì hàm nhận vào một danh sách nên chúng ta sẽ xác định các trường hợp tiêu chuẩn cho danh sách:

```haskell
reverse' :: [a] -> [a]
reverse' []     =
reverse' (x:xs) =
```

Nghịc đảo của danh sách rỗng cũng là một danh sách rỗng. Vì vậy, đó là một trong những trường hợp đơn giản. Và đây cũng là trường hợp cơ sở vì nó không có đệ quy:

```haskell
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) =
```

Và bây giờ, nếu chúng ta lấy phần tử đầu tiên, đặt nó ở cuối và tiếp tục làm như vậy cho đến khi đến cuối danh sách ban đầu thì nó sẽ bị đảo ngược! Vì vậy, chúng ta chỉ cần lấy `x` , đặt nó ở cuối và thực hiện đệ quy tương tự cho đến khi hết phần tử, đó là trường hợp đệ quy của chúng ta:

```haskell
reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse' [1,2,3,4,5]
reverse' "stressed" -- What's the reverse of stressed?
```

```
[5,4,3,2,1]

"desserts"
```

Được rồi. Chúng tôi đã xem những ví dụ đơn giản. Bây giờ hãy làm gì đó phức tạp hơn một chút:

#### `drop'` : Xóa `n` phần tử đầu tiên khỏi danh sách

Vậy nên, nó nhận vào một số nguyên và một danh sách rồi trả về một danh sách. Và bởi vì, về nguyên tắc, chúng ta sẽ không thao tác trên các phần tử của danh sách, nên chúng ta có thể sử dụng kiểu đa hình như thế này:

```haskell
drop' :: Int -> [a] -> [a]
```

OK! Một điều mới là bây giờ chúng ta có hai đối số khác nhau để xem xét.

Cách để làm việc này là liệt kê tất cả các tổ hợp mẫu tiêu chuẩn có thể có. Vì chúng ta có các số nên trước hết, chúng ta tính đến mẫu cho số `0` và cho một số khác bất kỳ. Và chúng ta có danh sách nên chúng ta cần tính đến mẫu cho danh sách rỗng và danh sách không rỗng.

Vì vậy chúng ta có:

```haskell
drop' :: Int -> [a] -> [a]
drop' 0 []     =
drop' 0 (x:xs) =
drop' n []     =
drop' n (x:xs) =
```

Như bạn có thể thấy, có nhiều thứ cần xem xét hơn. Nhưng nó không có nghĩa là công việc trở nên khó khăn hơn. Bây giờ, hãy nghĩ về từng trường hợp riêng lẻ.

1. Nếu chúng ta loại bỏ `0` phần tử khỏi danh sách rỗng, điều đó có nghĩa là kết quả sẽ là một danh sách rỗng.
2. Nếu chúng ta loại bỏ `0` phần tử khỏi danh sách không rỗng, chúng ta sẽ trả về chính danh sách đó.
3. Nếu chúng ta loại bỏ `n` phần tử khỏi danh sách trống, chúng ta có thể trả về lỗi hoặc danh sách rỗng. Chúng tôi chọn trả về danh sách rỗng.

Thay thế chúng trong các định nghĩa:

```haskell
drop' :: Int -> [a] -> [a]
drop' 0 []     = []
drop' 0 (x:xs) = x:xs
drop' n []     = []
drop' n (x:xs) =
```

Thế đấy. Chúng ta đã hoàn thành 3 trong số 4 trường hợp. Bây giờ, khi chúng ta muốn loại bỏ `n` phần tử khỏi một danh sách không rỗng thì sao?

Chúng ta đã tách phần tử đầu tiên ra khỏi danh sách. Vì vậy, nếu chúng ta loại bỏ nó đi thì sẽ bớt đi một phần tử cần loại bỏ. Nhưng nếu chúng ta chỉ làm điều gì đó như `drop n xs` , hàm sẽ tiếp tục loại bỏ các phần tử cho đến khi danh sách rỗng.

May mắn thay, có một giải pháp dễ dàng. Nếu chúng ta gọi đệ `drop'` với `xs` , chúng ta sẽ loại bỏ một phần tử trong mỗi lần gọi đệ quy. Vì vậy, chúng tôi có thể trừ `1` từ `n` cho mỗi lần gọi để giữ cho nó được đồng bộ. Bằng cách đó, nếu có nhiều hơn `n` phần tử, chúng ta sẽ dừng đệ quy khi đạt đến `n = 0` :

```haskell
drop' :: Int -> [a] -> [a]
drop' 0 []     = []
drop' 0 (x:xs) = x:xs
drop' n []     = []
drop' n (x:xs) = drop' (n - 1) xs
```

Được rồi. Chúng ta có một hàm hoạt động. Nhưng có một số điều cần được cải thiện:

1. Cả hai trường hợp lấy danh sách rỗng đều trả về danh sách rỗng. Vì vậy chúng ta có thể bỏ qua `Int` trong những trường hợp đó.
2. Ở trường hợp thứ hai, chúng ta chỉ chuyển giá trị list đầu vào sang, nên không cần khớp mẫu.
3. Chúng ta không sử dụng `x` trong định nghĩa đệ quy nên cũng có thể bỏ qua nó.

Thực hiện những thay đổi đó, chúng ta nhận được:

```haskell
drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' 0 xs     = xs
drop' n (_:xs) = drop' (n - 1) xs
```

Có vẻ như chúng ta đã đi đến định nghĩa `drop` cuối cùng. Nhưng chưa hẳn. Điều gì xảy ra nếu `n < 0` ? Về mặt lý thuyết, nó không có ý nghĩa gì cả. Nhưng trong thực tế, ai đó có thể đủ điên rồ để thử nó!

Trong trường hợp đó, hàm hiện tại của chúng ta sẽ tiếp tục loại bỏ từng phần tử một cho đến khi hết vì chúng ta sẽ không bao giờ đạt được `n = 0` .

Đó có thể là một cách để giải quyết trường hợp đó. Nhưng theo trực giác, bạn sẽ nghĩ rằng việc loại bỏ một số âm các phần tử sẽ có tác dụng tương tự như việc loại bỏ 0 phần tử.

Vì vậy chúng ta phải điều chỉnh định nghĩa của mình để phù hợp với điều đó. Và để làm được điều đó, chúng ta có thể thay đổi trường hợp xử lý `n == 0` thành xử lý `n <= 0` bằng cách gắn số vào biến `n` và sử dụng guard để kiểm tra thuộc tính mong muốn.

Như thế này:

```haskell
drop' :: Int -> [a] -> [a]
drop' _ []           = []
drop' n xs | n <= 0  = xs
drop' n (_:xs)       = drop' (n - 1) xs


drop' (-3) [1,2,3]

yesYouDo :: String -> String
yesYouDo = ("Ok, I do"++) . drop' 7

yesYouDo "I don't like chocolate."
yesYouDo "I don't like to write silly examples."
```

```
[1,2,3]

"Ok, I do like chocolate."

"Ok, I do like to write silly examples."
```

Và bây giờ hàm này đã hoạt động như dự định!

#### `take'` : Lấy (và trả về) `n` phần tử đầu tiên từ danh sách

Hàm này tương tự một cách kỳ lạ với `drop'` . Nó nhận vào một số nguyên và một danh sách rồi trả về một danh sách. Nhưng lần này, danh sách chứa tất cả các phần tử từ phần tử đầu tiên cho đến `n` . Vì vừa xử lý một trường hợp tương tự rồi, nên chúng ta sẽ thực hiện luôn bước đầu tiên và bước thứ hai:

```haskell
take' :: Int -> [a] -> [a]
take' 0 []     =
take' 0 (x:xs) =
take' n []     =
take' n (x:xs) =
```

Tương tự như trước, hãy suy nghĩ về từng trường hợp riêng lẻ:

1. Nếu chúng ta lấy `0` phần tử từ một danh sách rỗng, điều đó có nghĩa là kết quả sẽ là một danh sách rỗng.
2. Nếu chúng ta lấy `0` phần tử từ một danh sách không rỗng thì chúng ta không lấy gì cả, vì vậy chúng ta trả về một danh sách rỗng.
3. Nếu chúng ta lấy `n` phần tử từ một danh sách rỗng, chúng ta có thể trả về một lỗi hoặc một danh sách rỗng. Chúng ta chọn trả về danh sách rỗng.

Vì vậy, thay thế thành:

```haskell
take' :: Int -> [a] -> [a]
take' 0 []     = []
take' 0 (x:xs) = []
take' n []     = []
take' n (x:xs) =
```

Chà, thật dễ dàng. Bây giờ, đối với trường hợp đệ quy. Giống như lần trước, chúng ta cũng cần giảm `n` đi một ở mỗi bước. Tuy nhiên, không giống như lần trước, bây giờ chúng ta muốn giữ lại các phần tử ở mỗi bước. Và có một cách dễ dàng để làm điều đó.

Chúng ta có thể thêm chúng vào một danh sách mới. Danh sách này sẽ lớn dần lên cho đến khi chúng ta đạt đến `n = 0` hoặc hết phần tử trong danh sách:

```haskell
take' :: Int -> [a] -> [a]
take' 0 []     = []
take' 0 (x:xs) = []
take' n []     = []
take' n (x:xs) = x : take' (n-1) xs
```

Bây giờ, chúng ta có thể đơn giản hóa biểu thức:

1. Nếu `n = 0` , chúng ta không quan tâm đến danh sách. Dù thế nào chúng ta cũng sẽ trả lại một danh sách rỗng.
2. Nếu danh sách rỗng thì chúng ta không quan tâm đến con số. Dù thế nào thì chúng ta cũng sẽ trả về một danh sách rỗng.

Dịch sang mã:

```haskell
take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs
```

Chúng tôi gặp vấn đề tương tự như với `drop` . Theo trực giác, việc lấy một số âm phần tử sẽ thực hiện tương tự như lấy 0 phần tử. Nó không nên trả lại toàn bộ danh sách.

May mắn thay, chúng ta đã biết cách giải quyết vấn đề này. Tương tự như với định nghĩa của `drop` :

```haskell
take' :: Int -> [a] -> [a]
take' n _      | n <= 0 = []
take' _ []              = []
take' n (x:xs)          = x : take' (n-1) xs

take' 3 [1,2,3,4,5]
take' (-3) [1,2,3,4,5]
```

```
[1,2,3]

[]
```

#### `map'` : Hàm bậc cao áp dụng một hàm cho mọi phần tử trong danh sách

Như mọi khi, hãy bắt đầu với kiểu. Chúng ta sẽ có một hàm và một danh sách và sẽ trả về một danh sách. Bởi vì chúng ta không biết hàm sẽ được truyền dưới dạng đối số nên chúng ta sẽ sử dụng các biến kiểu đa hình. Vì vậy, kiểu là:

```haskell
map' :: (a -> b) -> [a] -> [b]
```

Bây giờ hãy liệt kê các trường hợp. Đối với hàm thì chỉ có một trường hợp. Bạn nhận được hàm. Vì vậy, xem xét các trường hợp "tiêu chuẩn" cho danh sách, chúng ta nhận được:

```haskell
map' :: (a -> b) -> [a] -> [b]
map' f []     =
map' f (x:xs) =
```

Nếu chúng ta không có phần tử nào trong danh sách, chúng ta chỉ trả về danh sách rỗng. Đó sẽ là trường hợp cơ sở của chúng ta. Ngoài ra, chúng ta sẽ không sử dụng hàm trong trường hợp này, nên có thể bỏ qua nó:

```haskell
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) =
```

Bây giờ đối với trường hợp đệ quy, chúng ta phải áp dụng hàm `f` cho mọi phần tử và trả về danh sách. Vì vậy, chúng ta có thể áp dụng `f` cho phần tử đầu tiên ( `x` ) và thêm nó vào trước lệnh gọi đệ quy của `map'` cho phần còn lại của danh sách ( `xs` ):

```haskell
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs


map' (+1) [1,2,3,4]
map' (++"!") ["Hey","Ho","Let's go"]
```

```
[2,3,4,5]

["Hey!","Ho!","Let's go!"]
```

Đây là một hàm cực kỳ hữu ích. Bạn sẽ sử dụng nó khá thường xuyên!

Bây giờ, hãy thực hiện một định nghĩa đệ quy cuối cùng trước khi tìm hiểu về các hàm fold!

#### `filter'` : Lọc các phần tử của danh sách không thỏa mãn hàm kiểm tra.

Chúng tôi đã sử dụng hàm này khá nhiều. Vì vậy, bạn biết nó hoạt động như thế nào. Nó nhận một hàm kiểm tra và một danh sách rồi trả về một danh sách chỉ có các phần tử thỏa mãn hàm kiểm tra đó:

```haskell
filter' :: (a -> Bool) -> [a] -> [a]
```

Bây giờ, nếu chúng ta liệt kê các trường hợp, tham số đầu tiên là một hàm, do đó chỉ có một trường hợp và tham số thứ hai là một danh sách, vì vậy nó có thể là danh sách rỗng hoặc danh sách không rỗng:

```haskell
filter' :: (a -> Bool) -> [a] -> [a]
filter' p []     =
filter' p (x:xs) =
```

Vì chúng ta không có phần tử cần lọc trong trường hợp đầu tiên nên chúng tôi trả về một danh sách rỗng. Và bởi vì chúng ta sẽ không sử dụng hàm kiểm tra nên chúng ta có thể bỏ qua nó. Bắt đầu cảm thấy dễ dàng rồi phải không?

```haskell
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' p (x:xs) =
```

Bây giờ chúng ta hãy giải quyết trường hợp đệ quy.

Trong trường hợp này, chúng ta có hai tình huống. Một là phần tử đó thỏa mãn hàm kiểm tra, hai là không thỏa mãn. Chúng ta có thể biểu diễn điều này theo nhiều cách khác nhau. Tôi thích sử dụng guards hơn:

```haskell
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' p (x:xs)
    | p x       =
    | otherwise =
```

Vì vậy, nếu hàm kiểm tra `p` được áp dụng cho phần tử đầu tiên `x` trả về `True` , thì chúng ta sẽ thêm phần tử đó vào danh sách mà chúng ta sẽ trả về cuối cùng. Ngược lại, thì không thêm. Và trong cả hai trường hợp, chúng ta áp dụng đệ quy `filter'` cho các phần tử còn lại ( `xs` ).

Đưa vào code, chúng ta nhận được:

```haskell
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs
    

filter' (==True) [True,False,True,True,False]
filter' ('!' `elem`) ["Hey!", "How are you?"]
filter' (\x -> x**2 < 37) [1,2,3,4,5,6,7,8,9,10]
```

```
[True,True,True]

["Hey!"]

[1.0,2.0,3.0,4.0,5.0,6.0]
```

Và thế là xong! Bạn có thể lọc đi!

Được rồi. Chúng tôi đã tạo đủ hàm đệ quy để bắt đầu nhận thấy một số mẫu. Vì vậy, hãy nói về điều đó.

## Trích rút mẫu cho `foldr`

Hãy xem các hàm được định nghĩa trước đó. Xem liệu bạn có thể phát hiện ra một mẫu không:

```haskell
sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs
```

```haskell
product' :: [Int] -> Int
product' []     = 1
product' (x:xs) = x * product' xs
```

```haskell
and' :: [Bool] -> Bool
and' []     =  True
and' (x:xs) =  x && and' xs
```

Như bạn có thể thấy, có một mẫu lặp lại trong mọi hàm!:

1. Có một trường hợp cơ sở cho danh sách rỗng trả về giá trị không đệ quy.
2. Có một trường hợp đệ quy cho một danh sách không rỗng, lấy giá trị đầu tiên của danh sách và áp dụng một hàm để kết hợp nó với lệnh gọi đệ quy xử lý phần còn lại của danh sách.

Mẫu này có tên! Nó được gọi là "đệ quy nguyên thủy" - primitive recursion.

Bây giờ, bạn đã biết cần làm gì. Chúng ta sẽ trích rút mẫu này thành một hàm riêng! Nhưng trước tiên, hãy lưu ý rằng mẫu này giả định hàm kết hợp các giá trị trong trường hợp đệ quy là một toán tử. Để tổng quát hơn, chúng ta hãy sửa đổi chúng để sử dụng các hàm tiền tố trước khi trích xuất:

```haskell
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = (+) x (sum' xs)
```

```haskell
product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = (*) x (product' xs)
```

```haskell
and' :: [Bool] -> Bool
and' []     =  True
and' (x:xs) =  (&&) x (and' xs)
```

Chúng ta sẽ gọi hàm trừu tượng này là `foldr` (duh) vì chúng ta sẽ gấp danh sách từ bên phải sang. Bạn sẽ hiểu ý tôi ngay thôi.

Như mọi khi, (đầu tiên, chúng ta bắt đầu với kiểu. Chúng ta cần 3 đối số:

1. Hàm kết hợp các phần tử của danh sách. Nó cần nhận vào hai phần tử và tạo một phần tử mới.
2. Một giá trị cơ sở để bắt đầu.
3. Một danh sách.

Lưu ý rằng các phần tử bên trong danh sách có thể là bất cứ thứ gì, nhưng không nhất thiết phải cùng kiểu với kết quả. (Chúng ta không biết hàm này sẽ làm gì.) Vì vậy, chúng ta sẽ sử dụng kiểu `a` cho các phần tử của danh sách và kiểu `b` cho kết quả. Và từ đó, giá trị cơ sở phải thuộc kiểu `b` và hàm phải thuộc kiểu `a -> b -> b` .

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
```

Được rồi, bây giờ hãy biến mẫu này thành hàm riêng đại diện cho nó. Hãy bắt đầu bằng cách biểu diễn mẫu và chúng ta sẽ bắt đầu từ đó:

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] =  -- base value
foldr f v (x:xs) = --function combining value and recursion
```

Chúng ta đã có giá trị cơ sở ( `v` ). Đó là một trong những đối số. Và lệnh gọi đệ quy chỉ áp dụng hàm `f` cho `x` và lệnh gọi đệ quy của `foldr` nhưng với `xs` thay vì danh sách ban đầu. Vì vậy, chúng ta có thể làm như sau:

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v [] =  v
foldr f v (x:xs) = f x (foldr f v xs)
```

Xong! Chúng ta vừa trích xuất mẫu "đệ quy nguyên thủy"!

Để chứng minh rằng nó thực sự giống như cũ, chúng ta sẽ truyền các tham số cần thiết để tạo hàm tính `sum` và thực hiện một ví dụ:

```haskell
-- same as: sum [1,2,3,4]
foldr (+) 0 [1,2,3,4] = (+) 1 (foldr (+) 0 [2,3,4])
                      = (+) 1 ((+) 2 (foldr (+) 0 [3,4]))
                      = (+) 1 ((+) 2 ((+) 3 (foldr (+) 0 [4])))
                      = (+) 1 ((+) 2 ((+) 3 ((+) 4 (foldr (+) 0 []))))
                      = (+) 1 ((+) 2 ((+) 3 ((+) 4 0))) -- 1 + ( 2 + ( 3 + ( 4 + 0 )))
                      = (+) 1 ((+) 2 ((+) 3 4)) -- 1 + ( 2 + ( 3 + 4 ))
                      = (+) 1 ((+) 2 7) -- 1 + ( 2 + 7 )
                      = (+) 1 9 -- 1 + 9
                      = 10
```

Hoạt động hoàn hảo!

Bây giờ, chúng ta có thể thay thế nó trong các định nghĩa trước đây để có được mã rõ ràng và ngắn gọn hơn nhiều:

```haskell
sum' :: [Int] -> Int
sum' = foldr (+) 0 -- We partially apply foldr


product' :: [Int] -> Int
product' = foldr (*) 1


and' :: [Bool] -> Bool
and' = foldr (&&) True
```

Nếu, khi định nghĩa một hàm đệ quy, bạn phát hiện ra rằng mình đang sử dụng mẫu này, hãy sử dụng `foldr` thay thế! Bằng cách đó, mọi người (kể cả bạn sau hai tháng nữa) sẽ ngay lập tức hiểu hàm thực hiện gì mà không cần phải giải quyết đệ quy.

Nói về điều này, hàm `length'` gần như phù hợp hoàn hảo!:

```haskell
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = (+) 1 (length' xs)
```

Sự khác biệt duy nhất là chúng ta bỏ qua `x` và thay vào đó cộng vào một hằng. Nếu chúng ta có thể mã hóa cứng tham số đầu tiên của toán tử `+` ... thì thật hoàn hảo! Chà, tại sao chúng ta không tạo một hàm thực hiện chính xác điều đó và truyền nó thay vì `+` ? Chúng ta chỉ cần lấy hai tham số, bỏ qua tham số đầu tiên và thêm `1` vào tham số thứ hai! Chúng ta có thể dễ dàng làm điều đó với một hàm lambda nhanh:

```haskell
length' :: [a] -> Int
length' []     = 0
length' (x:xs) = (\_ n -> 1 + n) x (length' xs) --lambda could be simplified to (\_ -> (+) 1)

length' [1,2,3,4,5]
```

```
5
```

Và bùm! Chỉ cần như vậy, `length'` hoàn toàn phù hợp với mẫu này! Vì vậy chúng ta có thể thay thế nó bằng `foldr` :

```haskell
length' = foldr (\_ n -> 1 + n) 0

length' [1,2,3,4,5]
```

```
5
```

Như bạn có thể thấy, có sự linh hoạt nhất định. Hãy viết lại <code>reverse</code>  với `foldr` :

```haskell
reverse' :: [a] -> [a]
reverse' = foldr (\x xs -> xs ++ [x]) []

reverse' [1,2,3,4,5]
```

```
[5,4,3,2,1]
```

Có vẻ như chúng ta có thể sử dụng `foldr` cả ngày. Nhưng không phải tất cả đều màu hồng. Ví dụ: nếu sử dụng `reverse'` với một nghìn, mười nghìn hoặc thậm chí nhiều số hơn thì hậu quả của việc sử dụng `++` ngày càng lớn hơn.

Tại sao? Chà... hãy xem `++` được định nghĩa như thế nào trong thư viện cơ sở:

```haskell
(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys
```

Như bạn có thể thấy trong trường hợp đệ quy, mỗi lần chúng ta muốn nối hai danh sách, trước tiên, chúng ta sẽ duyệt qua tất cả các thành phần của danh sách đầu tiên, sau đó chúng ta thêm danh sách thứ hai vào cuối. Vì vậy, nếu chúng ta có một danh sách lớn hơn gấp 10 lần, chúng ta phải đợi gấp 10 lần để nó hoàn thành. Có nghĩa là phải mất thời gian tuyến tính theo số phần tử của danh sách đầu tiên.

Điều đó có ý nghĩa gì với chúng ta? Nó có nghĩa là trong phép đệ quy của `reverse'`, mỗi lần chúng ta muốn di chuyển một phần tử từ đầu đến cuối danh sách (mỗi lần thực hiện lệnh gọi đệ quy), chúng ta phải duyệt qua toàn bộ danh sách! Mỗi lần! Nếu danh sách đủ dài, bạn có thể chạy bộ trong khi chờ nó được đảo ngược!

Nhưng đừng lo lắng. Tôi sẽ không để nó dở dang như vậy. Có một giải pháp gọn gàng cho việc này. Nếu chúng ta có thể duyệt qua danh sách từ trái sang phải thay vì từ phải sang trái, chúng ta có thể sử dụng toán tử cons ( `:` ) thay vì `++` và trong mỗi lệnh gọi đệ quy, chúng ta sẽ thêm phần tử vào ngay đầu. Không cần phải duyệt qua toàn bộ danh sách!

Đến với hàm `foldl` !

## Hàm `foldl`

Về cơ bản, `foldl` hoạt động giống như `foldr` nhưng duyệt danh sách từ trái sang phải:

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] =  v
foldr f v (x:xs) = f x (foldr f v xs)


foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f v [] = v
foldl f v (x:xs) = foldl f (f v x) xs
```

Ví dụ: hãy xem điều gì xảy ra từng bước khi chúng ta thay thế `foldr` bằng `foldl` trong hàm `sum` :

(Lưu ý rằng đối số thứ hai tiếp tục tăng trong khi đối số thứ ba ngày càng nhỏ hơn.)

```haskell
foldl (+) 0 [1,2,3,4] = foldl (+) ((+) 0 1) [2,3,4]
                      = foldl (+) ((+) ((+) 0 1) 2) [3,4]
                      = foldl (+) ((+) ((+) ((+) 0 1) 2) 3) [4]
                      = foldl (+) ((+) ((+) ((+) ((+) 0 1) 2) 3) 4) []
                      = (+) ((+) ((+) ((+) 0 1) 2) 3) 4 -- ((( 0 + 1 ) + 2 ) + 3 ) + 4
                      = (+) ((+) ((+) 1 2) 3) 4 -- ((1 + 2 ) + 3 ) + 4
                      = (+) ((+) 3 3) 4 -- (3 + 3 ) + 4
                      = (+) 6 4 -- 6 + 4
                      = 10
```

Và đó là cách `foldl` hoạt động.

Và bởi vì bây giờ chúng ta có thể duyệt danh sách từ trái sang phải, chúng ta có thể sử dụng toán tử `:` (cons) để nối các giá trị thay vì `++` .

Xem xét khả năng này, chúng ta có thể viết `reverse'` như sau:

```haskell
reverse'' :: [a] -> [a]
reverse'' = foldl (\x y -> y:x) []  -- Same as: foldl (flip (:)) []

reverse'' [1,2,3,4,5]
```

```
[5,4,3,2,1]
```

Và bây giờ, chúng ta có thể so sánh tốc độ của hai hàm bằng cách đảo ngược danh sách từ 1 đến 10.000! Chạy riêng hai ô và thấy sự khác biệt về tốc độ:

(Chúng tôi sử dụng `sum` để tránh in toàn bộ danh sách)

```haskell
sum . reverse' $ [1..10000] -- With foldr and (++)
```

```
50005000
```

```haskell
sum . reverse'' $ [1..10000] -- With foldl and (:)
```

```
50005000
```

Một sự cải tiến ấn tượng! Nhưng đó không phải điều duy nhất khác biệt giữa `foldr` và `foldl` !

Cho đến giờ, chúng ta chưa gặp phải điều này vì, ví dụ, toán tử cộng ( `+` ) trả về cùng một cách:

```haskell
foldr (+) 0 [4,3,2,1] == foldl (+) 0 [4,3,2,1]
```

```
True
```

Tuy nhiên, đối với một số toán tử, thứ tự thực hiện phép tính có thể cho kết quả khác nhau tùy theo hướng thực hiện! Ví dụ: hãy xem xét `(-)` thay vì `(+)` :

```haskell
foldr (-) 0 [4,3,2,1] == foldl (-) 0 [4,3,2,1]
```

```
False
```

Điều này sai vì nếu chúng ta viết các phép tính ra một cách rõ ràng, chúng ta sẽ nhận được:

```haskell
foldl (-) 0 [4,3,2,1] = (((0-4)-3)-2)-1 = -10
```

trong khi

```haskell
foldr (-) 0 [4,3,2,1] = 4-(3-(2-(1-0))) = 2
```

Vì vậy, đó là một điều khác cần tính đến.

Và cuối cùng, có một hàm cuối cùng tôi muốn nói đến. Và đó là `foldl'` .

## Hàm `foldl'`

Tất cả các hàm mà chúng ta đã định nghĩa cho đến nay đều có `'` ở cuối vì chúng đã tồn tại trong Haskell và chúng ta không muốn xảy ra xung đột. Nhưng! `foldl'` cũng là một hàm đi kèm với Haskell và nó hoạt động hơi khác một chút so với `foldl` .

Trong cả hai trường hợp `foldr` và `foldl` , chúng ta thấy rằng chúng ta tiếp tục xếp chồng các biểu thức cho đến cuối. Và sau đó chúng ta rút gọn chúng. (Thực ra, Haskell thực hiện tất cả công việc, không phải chúng ta. Nhưng bạn hiểu ý tôi rồi đó.)

Điều này có nghĩa là nếu bạn cố gắng gấp một danh sách đủ lớn, bạn sẽ gặp phải exception `stack overflow` !

Nếu chúng ta chọn bất kỳ bước trung gian nào trong `foldr` :

```haskell
-- Same as:             (+) 1 ((+) 2 ((+) 3 (foldr (+) 0 [4])))
foldr (+) 0 [1,2,3,4] = 1 + (2 + (3 + (foldr (+) 0 [4])))
```

Chúng ta thấy rằng chúng ta không làm được gì nhiều cho `foldr` vì chúng ta không có một toán tử đơn lẻ có đủ cả hai đối số. Vì vậy, chúng ta luôn cần giải quyết hàm đệ quy trước.

Nhưng! Nếu chúng ta xem xét bước trung gian tương tự trong `foldl` :

```haskell
-- Same as:             foldl (+) ((+) ((+) ((+) 0 1) 2) 3) [4]
foldl (+) 0 [1,2,3,4] = foldl (+) (((0 + 1) + 2) + 3) [4]
```

Chúng ta hoàn toàn có thể rút gọn `(((0 + 1) + 2) + 3)` thành `6` trước khi tiếp tục đệ quy!

Và đó là những gì `foldl'` làm!

Để cho rõ: `foldl` và `foldl'` trả về cùng một kết quả! Sự khác biệt là ở chỗ `foldl'` rút gọn các biểu thức ở các bước trung gian. Vì vậy, nó hiệu quả hơn vì nó không tạo ra một "thunk" lớn!

Vì vậy, nếu chúng ta chạy một cái gì đó như thế này:

```haskell
foldl (+) 0 [1..1000000] -- Don't run it! I'm warning you!
```

Bạn sẽ nhận được ngoại lệ tràn ngăn xếp. Nhưng nếu bạn sử dụng `foldl'` thay thế:

```haskell
import Data.List

foldl' (+) 0 [1..1000000]  -- No problems!
```

```
500000500000
```

Bạn sẽ không gặp phải vấn đề gì.

Và điều này đặt ra một câu hỏi. Khi nào bạn nên sử dụng cái nào?

## Khi nào nên sử dụng `foldr` , `foldl` và `foldl'`

Thông thường, sự lựa chọn là giữa `foldr` và `foldl'` , vì `foldl` và `foldl'` giống nhau ngoại trừ các thuộc tính nghiêm ngặt của chúng. Vì vậy, nếu cả hai đều trả về kết quả, `foldl'` là cách hiệu quả hơn để đi đến kết quả đó vì nó không tạo ra thunk lớn.

Tuy nhiên, đó không phải là tất cả. Chúng ta sẽ đưa ra một số quy tắc chung từ hàm fold ít được sử dụng nhất đến được sử dụng nhiều nhất:

Sử dụng `foldl` :

- Hiếm khi.
- Nếu hàm kết hợp là lười biếng trong đối số đầu tiên của nó. ( `foldl` có thể trả về kết quả trong khi `foldl'` gặp một exception.)

Sử dụng `foldl'` :

- Khi danh sách mà nó được áp dụng lớn nhưng chắc chắn là hữu hạn, bạn không quan tâm đến sự đảo ngược ngầm (ví dụ: vì hàm kết hợp của bạn có tính giao hoán) và bạn tìm cách cải thiện hiệu suất mã của mình.
- Khi bạn thực sự muốn đảo ngược thứ tự của danh sách, ngoài việc có thể thực hiện một số chuyển đổi khác đối với các phần tử. (Tận dụng hiệu ứng đảo ngược ngầm.)

Sử dụng `foldr` :

- Khi chuyển đổi danh sách thành danh sách có các phần tử liên quan theo cùng một thứ tự.
- Khi chuyển đổi danh sách vô hạn thành danh sách vô hạn khác. (Nếu hàm được truyền là lười biếng trong đối số thứ hai của nó, `foldr` sẽ tạo ra kết quả một cách lười biếng, chỉ tính toán theo yêu cầu.)
- Khi hàm gấp có thể bị đoản mạch (chấm dứt sớm) bằng cách tạo ra kết quả không phụ thuộc vào giá trị của tham số tích lũy.
- Nếu bạn không chắc chắn.

Những quy tắc này không nhất thiết phải luôn được áp dụng. Và bởi vì việc giải thích tất cả lý do tại sao của những quy tắc này có thể tốn nhiều thời gian nên chúng tôi sẽ dành nó cho những người tò mò hoặc khi bạn cần nó. [Đây là thông tin thêm về chủ đề này](https://wiki.haskell.org/Foldr_Foldl_Foldl') .

# Đó là tất cả cho buổi học hôm nay!
