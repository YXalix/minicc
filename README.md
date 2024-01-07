## minicc

 > [!cite]
> 本课程基于Rui的[chibicc](https://github.com/rui314/chibicc)以及sushaoce的[rvcc](https://github.com/sunshaoce/rvcc), 使用Rust来实现简单的C子集
### Build
建议在Ubuntu 22.04 和 macOS (M1/M2/Intel) 上使用本项目: 
**For Ubuntu user**
- QEMU 模拟器安装
``` shell
# 安装编译所需的依赖包
sudo apt install autoconf automake autotools-dev curl libmpc-dev libmpfr-dev libgmp-dev \
              gawk build-essential bison flex texinfo gperf libtool patchutils bc \
              zlib1g-dev libexpat-dev pkg-config  libglib2.0-dev libpixman-1-dev libsdl2-dev \
              git tmux python3 python3-pip ninja-build
# 下载源码包
wget https://download.qemu.org/qemu-7.0.0.tar.xz
# 解压
tar xvJf qemu-7.0.0.tar.xz
# 编译安装并配置 RISC-V 支持
cd qemu-7.0.0
./configure --target-list=riscv64-softmmu,riscv64-linux-user  # 如果要支持图形界面，可添加 " --enable-sdl" 参数
make -j$(nproc)

# 请注意，qemu-7.0.0 的父目录可以随着你的实际安装位置灵活调整
# 在~/.bashrc中添加以下的内容
export PATH=$PATH:/path/to/qemu-7.0.0/build

# 更新~/.bashrc的更改
source ~/.bashrc

# 确认 QEMU 的版本
qemu-system-riscv64 --version
qemu-riscv64 --version
```
- RISC-V64 GCC 工具链
	- [Ubuntu 平台](https://static.dev.sifive.com/dev-tools/riscv64-unknown-elf-gcc-8.3.0-2020.04.1-x86_64-linux-ubuntu14.tar.gz)
	- [macOS 平台](https://static.dev.sifive.com/dev-tools/riscv64-unknown-elf-gcc-8.3.0-2020.04.1-x86_64-apple-darwin.tar.gz)
	- [Windows 平台](https://static.dev.sifive.com/dev-tools/riscv64-unknown-elf-gcc-8.3.0-2020.04.1-x86_64-w64-mingw32.zip)
	- [CentOS 平台](https://static.dev.sifive.com/dev-tools/riscv64-unknown-elf-gcc-8.3.0-2020.04.1-x86_64-linux-centos6.tar.gz)

**For macOS user**
- [Spike](https://github.com/riscv/riscv-isa-sim) 模拟器
``` shell 
brew tap riscv/riscv
brew install riscv-isa-sim
```
- 简易内核 [riscv-pk](https://github.com/riscv/riscv-pk)
- RISC-V64 GCC 工具链 如上
``` shell
# 运行方法如下
spike pk ./tmp
# 如果出现icu4c 版本不一致的情况, 需要手动切换其版本
brew tap petere/icu
brew install icu@{version you need}
```
### 测试
```
make test
```
### Framework
`ltex.rs` : 生成Token流
`AST.rs`: 定义了AST的各个类型
`parse.rs`: 生成AST
`IR.rs`: 根据传入的AST, 生成IR, 以及符号表用Hashmap实现
`codegen.rs`: 根据生成的IR, 翻译成汇编代码

### 未来的工作
添加IR优化, 目前是最原始的编译
- [ ] 添加多文件编译, 以及链接工作
- [ ] 添加IR优化, 目前是最原始的编译
	- [ ] 寄存器分配
- [ ] 参照LLVM IR来实现, 并调用LLVM后端