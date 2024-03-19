# Giới thiệu về

![](https://www.haskell.org/img/haskell-logo.svg)

## Nội dung

- Haskell là gì?
- Ngôn ngữ lập trình hàm
    - Phép kết hợp hàm
- Hiệu ứng rõ ràng (Pure)
- Cú pháp cơ bản
    - Căn lề và chú thích
    - Định nghĩa và sử dụng hàm
- Hệ thống kiểu của Haskell
- Tính lười biếng
- Các công cụ: GHC (GHCi), Cabal, Stack

## Cách sử dụng JupyterLab

- Mỗi bài học là một sổ tay Jupyter.
- Mỗi sổ Jupyter là một chuỗi các ô.
- Để thực thi một ô, hãy nhấn ⇧⏎ (Shift + Enter).
- Bạn có thể tương tác với mã bên trong các ô.
- Khi bạn đóng tab, mọi thay đổi sẽ bị mất.

## Haskell là gì?

Chúng ta sẽ xem xét từng tính chất của Haskell và trả lời câu hỏi này ở cuối bài giảng.

## Ngôn ngữ lập trình hàm

Haskell là một ngôn ngữ lập trình hàm.

Trong các ngôn ngữ lập trình mệnh lệnh, định nghĩa hàm là một chuỗi các câu lệnh mệnh lệnh.

Trong các ngôn ngữ lập trình hàm, định nghĩa hàm là **cây biểu thức ánh xạ các giá trị này tới các giá trị khác**.

**Các chương trình được xây dựng bằng cách *áp dụng* và *kết hợp* các hàm**.

### Phép kết hợp hàm

**Kết hợp hàm là hành động *truyền* *kết quả* của hàm này làm *đầu vào* của hàm khác, tạo ra một hàm hoàn toàn mới**

Giống như cách kết hợp thông thường của các hàm trong toán học, **kết quả của mỗi hàm được truyền vào làm đối số của hàm tiếp theo** và kết quả của hàm cuối cùng là kết quả tổng thể.

Ví dụ: giả sử chúng ta có hai hàm $f$ và $g$:

$$y = f(x)$$ 
$$z = g(y)$$

Việc kết hợp chúng có nghĩa là trước tiên chúng ta tính $f(x)$ để có được $y$, sau đó sử dụng $y$ làm đối số để tính $g(y)$, ta thu được $z$.

Tạo một hàm đi từ $x$ đến $z$:

$$z = g(f(x))$$

Và đó là cách chúng ta có thể tạo ra **các hàm phức tạp tùy ý bằng cách kết hợp các hàm đơn giản.**

Ví dụ, nếu chúng ta có:

- Một hàm nhận vào một bảng tính và trả về danh sách người chơi chứa trong đó.
- Một hàm nhận danh sách người chơi và trả về danh sách người chơi được sắp xếp theo điểm số.
- Và một hàm nhận danh sách người chơi đã sắp xếp và trả về 3 người đứng đầu.

Chúng ta có thể tạo **một hàm duy nhất nhận vào bảng tính và trả về 3 người chơi giỏi nhất** bằng cách kết hợp ba hàm đó.

Ngoài ra, Haskell còn có các hiệu ứng rõ ràng (còn gọi là thuần túy 👼)!

## Hiệu ứng rõ ràng (hàm thuần túy)

Các ngôn ngữ lập trình hàm thuần túy coi **toàn bộ việc tính toán là sự đánh giá các hàm toán học** .

Trong toán học, biểu thức $y = x + 1$ có nghĩa là giá trị của $y$ là một hàm phụ thuộc vào $x$.

Đối với một giá trị $x$ cụ thể, giá trị của $y$ sẽ luôn giống nhau.

Bất kể bạn đang ở Ý hay Tây Ban Nha, đó là năm 1994 hay 2022, hay bạn có các phương trình khác trong sổ tay. $y$ sẽ quan tâm đến giá trị của $x$ và không quan tâm đến giá trị nào khác.

Trong các ngôn ngữ lập trình hàm thuần túy, các hàm thuần túy **chỉ phụ thuộc vào các đối số của chúng** và **không tương tác với bất kỳ trạng thái cục bộ hoặc toàn cục nào** . (Điều này được gọi là "không có *hiệu ứng phụ* .")

Có nghĩa là **đối với một đầu vào cụ thể, một hàm sẽ luôn trả về cùng một giá trị.**

Nghe thì có vẻ là một ý tưởng tồi nhưng nếu bạn nghĩ kỹ thì nó lại có một số hệ quả cực kỳ thuận lợi:

- Nó cho phép bạn dễ dàng suy luận và chứng minh rằng một hàm là đúng.
- Trong Haskell, người ta luôn có thể “thay thế bằng nhau”, giống như bạn đã học trong lớp học đại số.
- Làm cho mã của bạn ít bị lỗi hơn đáng kể.
- Việc tính toán song song/đồng thời dễ dàng hơn. (Nếu không có sự phụ thuộc dữ liệu giữa hai biểu thức thuần túy thì chúng có thể được thực hiện song song và chúng không thể can thiệp lẫn nhau.)
- Và còn nhiều lợi ích khác nữa...

**Haskell hoạt động như một ngôn ngữ thuần túy, nhưng cho phép các hiệu ứng phụ (giao tiếp mạng, I/O, v.v.) bằng cách gắn nhãn rõ ràng cho chúng trong hệ thống kiểu.** Chúng ta sẽ thấy cách thực hiện trong các bài học sau. (Điều này được gọi là "*explicit effects*" - hiệu ứng rõ ràng).

Trước khi tiếp tục với những tính chất khác, hãy xem Haskell thực tế trông ra sao.

## Cú pháp cơ bản

### Chú thích code

```haskell
-- Sử dụng dấu gạch ngang kép để chú thích trong một dòng mã.

{-
Sử dụng dấu ngoặc nhọn với một dấu gạch ngang để
mở và đóng
chú thích nhiều dòng.
-}
```

### Căn lề

**Haskell rất nhạy cảm với khoảng trắng đầu dòng**. Điều đó có nghĩa là dấu cách, tab và dòng mới rất quan trọng khi viết code.

Nguyên tắc vàng là:

<p></p>
<blockquote>Đoạn code của một biểu thức phải được thụt vào sâu hơn phần đầu của biểu thức đó (ngay cả khi biểu thức không phải là phần tử ngoài cùng bên trái của dòng).</blockquote>
<p>
</p>

Chúng ta sẽ xem xét các ví dụ trong các bài học sau.

### Định nghĩa hàm

Haskell là ngôn ngữ lập trình hàm có nghĩa là bạn sẽ viết rất nhiều hàm. Vì vậy, đó là nơi chúng ta sẽ bắt đầu.

Đây là biểu thức để xác định hàm kiểm tra xem một số có lớn hơn 18 hay không:

```haskell
greaterThan18 x = x > 18
```

- `greaterThan18` là tên của hàm. Hãy chọn một cái tên giúp bạn dễ dàng biết hàm của bạn làm gì.
- `x` là tham số.
- Toán tử `=` gán biểu thức `x > 18` cho name `greaterThan18` .

Bên trái dấu `=` chúng ta viết tên hàm và các tham số. Và ở bên phải là biểu thức được chứa bởi hàm.

### Sử dụng hàm

Để sử dụng hàm `greaterThan18`, chúng ta chỉ cần viết tên, dấu cách và một số:

```haskell
greaterThan18 3
```

Hàm được thực thi, Haskell thay thế tất cả `x` bằng `30` và `greaterThan18 30` trở thành `30 > 18` . Sau đó, nó đánh giá biểu thức và trả về `True` .

### Các ví dụ khác

```haskell
-- A function that adds 6 numbers:
add6numbers u v w x y z = u + v + w + x + y + z
add6numbers  1 2 3 4 5 6  -- 21
```

```haskell
-- A function that calculates the volume of a cylinder
volumeOfACylinder r h = pi * r^2 * h  -- pi represents the number π, and it comes with Haskell
volumeOfACylinder 3 10
```

```haskell
-- A function that takes the temperature in Fahrenheit and returns it in Celsius
fToC x = (x - 32) * 5 / 9
fToC 212  -- 100
```

### Những điểm đáng chú ý

- Các tham số được phân tách bằng dấu cách.
- Mọi thứ sau `=` là phần thân của hàm.
- Chữ cái đầu tiên của tên hàm phải là chữ thường.
- Chúng ta sử dụng dấu ngoặc đơn để chỉ định tính toán ưu tiên, giống như trong toán học.

## Hệ thống kiểu của Haskell

Chúng ta sẽ tìm hiểu sâu về hệ thống kiểu của Haskell trong bài 2. Dưới đây, bạn sẽ xem một số điều cơ bản.

Kiểu là **các thuộc tính ràng buộc các giá trị mà một đoạn code có thể có** . Ví dụ: nếu bạn chỉ định một dữ liệu nào đó là một con số thì dữ liệu đó có thể có bất kỳ giá trị nào sau đây:

```
  32

  9999695939294

  0.5
```

Tuy nhiên, nếu bạn cố gắng thêm một ký tự vào đó, như thế này: `6A3` (thay vì `63` ), trình biên dịch/thông dịch sẽ hét vào mặt bạn.

Những gì trình biên dịch/trình thông dịch của bạn vừa làm được gọi là **"type checking" - kiểm tra kiểu dữ liệu**. Một số ngôn ngữ chặt chẽ trong kiểm tra kiểu, một số thì lỏng lẻo hơn.

```
  6A3
```

### Type checking - Kiểm tra kiểu dữ liệu

**Kiểm tra kiểu là quá trình xác minh và thực thi các ràng buộc của kiểu.**

**Điều đó có nghĩa là gì?** Nó có nghĩa là mỗi kiểu có những ràng buộc riêng (Ví dụ: bạn không thể làm toán bằng các chữ cái.) và quá trình này sẽ kiểm tra xem những ràng buộc đó có được tuân thủ hay không.

**Tại sao lại làm vậy?** Để tránh những sai sót có thể phòng ngừa.

### Dynamically typed languages - Ngôn ngữ có kiểu dữ liệu động

Nếu trong chương trình, bạn muốn cộng vài số với nhau và một trong số chúng có một chữ cái, chương trình sẽ không biết phải làm gì và nó sẽ gặp lỗi. Đó là những lỗi có thể phòng ngừa được và trình biên dịch/thông dịch sẽ giúp bạn điều đó.

Thông thường, việc này được thực hiện tự động. Nhưng không phải tất cả các ngôn ngữ đều làm điều này theo cùng một cách. Chúng được phân loại thành 2 loại chính liên quan đến KHI NÀO các kiểu trong chương trình được kiểm tra: Ngôn ngữ có kiểu động và Ngôn ngữ có kiểu tĩnh.

**Ngôn ngữ có kiểu động sẽ kiểm tra kiểu trong thời gian chạy - run-time** .

Run-time là việc cuối cùng bạn làm với một chương trình. Đây là giai đoạn bạn chạy chương trình của mình để kiểm tra hoặc sử dụng nó.

Các ví dụ phổ biến về ngôn ngữ có kiểu động bao gồm JavaScript, Python, Objective-C và PHP.

### Statically typed languages - Ngôn ngữ có kiểu dữ liệu tĩnh

**Các ngôn ngữ có kiểu dữ liệu tĩnh kiểm tra kiểu trong thời gian biên dịch - compile-time** .

Nghĩa là bạn sẽ biết liệu kiểu dữ liệu của mình có sai hay không ngay khi bạn biên dịch chương trình. Điều này dẫn đến code an toàn hơn và tối ưu hơn.

Các ví dụ phổ biến về ngôn ngữ có kiểu tĩnh bao gồm Java, C và C++.

### Hệ thống kiểu của Haskell

**Haskell có kiểu tĩnh**. Và trong Haskell, **mọi biểu thức đều có kiểu**.

Nhưng đừng lo lắng, bạn không cần phải xác định thủ công các kiểu của mọi biểu thức vì trình biên dịch của Haskell rất giỏi trong việc **suy luận kiểu - "type inference"**.

**Suy luận kiểu cho phép Haskell tự suy ra các kiểu dữ liệu**.

Nếu bạn viết một cái gì đó như `3 + 4` , Haskell sẽ biết rằng kết quả của biểu thức đó là một số và xử lý nó như vậy mà không cần bạn chỉ định kiểu. (Nó cũng hoạt động với các biểu thức phức tạp hơn. Xem các ví dụ trước.)

Điều đó cho phép trình biên dịch **hiểu và suy luận *khá nhiều* về chương trình của bạn** . Cung cấp cho bạn một trợ thủ phát hiện lỗi khá hiệu quả.

Dù không cần thiết đối với trình biên dịch, **nhưng việc viết ra chữ ký kiểu của các hàm và biểu thức cấp cao được coi là một thói quen tốt trong thực hành**, giúp cải thiện khả năng đọc code.

Nếu code quá mơ hồ để trình biên dịch suy luận kiểu, nó sẽ yêu cầu bạn chỉ định kiểu.

## Tính lười biếng

**Haskell lười biếng. Điều này có nghĩa là nó sẽ không đánh giá các biểu thức cho đến khi cần sử dụng kết quả của chúng**

Ví dụ về sự lười biếng của Haskell trong thực tế:

- Chúng ta có thể sử dụng danh sách vô hạn.

```haskell
giveMe x = take x [1..] -- [1..] is an infinite list of natural numbers that starts at 1.
giveMe 7
```

- Haskell sẽ không đánh giá các biểu thức nếu chúng không cần thiết

```haskell
cheapComputation = 7
expensiveComputation = sum [1..10000000] -- sum is a function that takes a list and returns the sum of all the elements. This will crash the kernel.
if cheapComputation > 5 || expensiveComputation > 5 then "Done" else "This won't ever show because expensiveComputation is always bigger than 5"
-- Try running this cell with cheapComputation being bigger and smaller than 5.
-- When cheapComputation > 5, expensiveComputation isn't evaluated because it is not needed.
```

## Vậy Haskell là gì?

#### Haskell là một ngôn ngữ lập trình hàm **có tính lười biếng**, có kiểu dữ liệu tĩnh với **các hiệu ứng rõ ràng** và hàm của nó trông như thế này:

```haskell
volumeOfACylinder r h = pi * r^2 * h
```

<div class="alert alert-block alert-info"> <b>Lưu ý:</b> Haskell có các thuộc tính quan trọng khác (như kiểu dữ liệu đại số, lớp kiểu, suy luận kiểu, đa hình, ...) mà chúng ta sẽ đề cập trong các bài học sau.</div>

( *Tính lười biếng* và *hiệu ứng rõ ràng* là hai trong số những đặc tính độc đáo của Haskell. Đó là lý do tại sao chúng được in đậm.)

## Công cụ

### Đôi lời về Cabal và Stack

Khi tìm hiểu về Haskell, bạn sẽ thường gặp các thuật ngữ Cabal và Stack.

**Đây là những hệ thống quản lý thư viện và chương trình**. Chúng giúp làm việc với các thư viện dễ dàng hơn.

Chúng ta sẽ sử dụng Cabal trong khóa học này và chúng tôi sẽ giải thích cách sử dụng nó ở giai đoạn sau.

### GHC và GHCi

**GHC (Glasgow Haskell Compiler) là trình biên dịch và môi trường tương tác cho Haskell** . Sử dụng GHC chúng ta có thể:

- Biên dịch chương trình và thực thi chúng như bất kỳ ứng dụng nào khác.
- Đánh giá các biểu thức Haskell ngay lập tức bằng cách sử dụng môi trường tương tác do GHC cung cấp (GHCi).

Để sử dụng GHCi, hãy mở terminal trong môi trường GitPod từ xa mà chúng ta đã chuẩn bị và nhập `ghci` .

Sử dụng `:l relative/path.hs` trong GHCi để tải một tệp, tương tác với các nội dung của nó, và `:q` để thoát.

**LƯU Ý:** Nếu bạn muốn cài đặt GHC và GHCi trên máy tính của mình, bạn có thể làm theo hướng dẫn trên www.haskell.org/ghcup/. Hướng dẫn được cung cấp cho Windows, Mac và Linux.
