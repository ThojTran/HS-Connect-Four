LTH/
│
├── .stack-work/                # Thư mục build tự động của Stack
│
├── app/                        # Chứa các entry point chạy chính
│   ├── Client/
│   │   └── Main.hs             # Main program của client
│   └── Server/
│       └── Main.hs             # Main program của server
│
├── data/                       # (Tuỳ chọn) Lưu dữ liệu, file config hoặc mẫu
│
├── src/                        # Source code chính của project
│   ├── Config.hs               # File cấu hình chung cho toàn project
│   │
│   ├── core/                   # Xử lý logic và quy tắc trò chơi
│   │   ├── Board.hs
│   │   ├── Logic.hs
│   │   ├── Player.hs
│   │   └── Types.hs
│   │
│   ├── Network/                # Quản lý phần mạng, giao tiếp client-server
│   │   ├── Client.hs
│   │   ├── Message.hs
│   │   └── Server.hs
│   │
│   └── Utils/                  # Các tiện ích hỗ trợ (xử lý song song, parser,…)
│       ├── Concurrency.hs
│       ├── Parser.hs
│       └── Serializer.hs
│
├── test/                       # Unit tests cho từng module
│   ├── GameLogicSpec.hs
│   └── NetworkSpec.hs
│
├── CHANGELOG.md
├── connect-four.cabal          # File cấu hình chính cho Haskell project
├── LICENSE
├── package.yaml
├── README.md
├── stack.yaml
└── stack.yaml.lock
