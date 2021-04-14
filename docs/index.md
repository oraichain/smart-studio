LOGIN
=========================

![Login Page](images/login.png)

Click Connect để đăng nhập bằng github

![connect-to-github](images/connect-to-github.png)

Sau khi đăng nhập thành công, user có thể click vào project đã tạo từ trước / tạo mới project dựa trên template có sẵn.

OPEN / CREATE A PROJECT
=========================

![recent-projects](images/recent-projects.png)

VD: Tạo 1 datasource contract.
![create-ds-smart-contract](images/create-ds-smart-contract.png)

Tên của contract mới tạo không được trùng với các dự án đã tạo từ trước.
Tên chỉ chứa ký tự a-z0-9, không chứa ký tự đặc biệt hoặc dấu cách

Sau khi nhập tên, Click Create để tạo mới

IDE
=========================
![greeting-started](images/greeting-started.png)

Màn hình IDE bao gồm:
- treeview bên trái:
  - các file rust: Sourcecode của smart contract
  - artifact: json schema và wasm file
- editor:
  - toolbar:
    - download: tải toàn bộ sourcecode về máy tính
    - build: dịch smartcontract ra wasm file
    - build schema: sinh file json schema
    - run tests: chạy mã kiểm thử

### Thao tác với IDE

Click item file rust bên trái để edit:
![code](images/code.png)

Click phải vào 1 file để đổi tên / xoá / tải file về máy tính
![file-op2](images/file-op2.png)
![file-op](images/file-op.png)


Để build source code ra wasm click Build button tại thanh toolbar
![build project](images/build.png)
![build-success](images/build-success.png)

Để build schema, click Build Schema tại thanh toolbar
Sau khi build schema thành công, 
![schema-success](images/build-schema.png)

### Simulate



### Deploy contract

Click chuột phải vào file wasm, chọn Deploy Contract
Màn hình mở ra trang keystation để thực hiện ký và deploy contract
![key-station](images/key-station.png)


