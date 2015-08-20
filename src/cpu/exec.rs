//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use cpu::reg::Regs;
use inst::{Inst, RuntimeInst};
use mem::Memory;

pub trait ExecCtx {
    type Mem: Memory;
    fn mem(&mut self) -> &mut Self::Mem;
    fn regs(&mut self) -> &mut Regs;
}

pub fn exec<M: Memory>(inst: &RuntimeInst, ctx: &mut ExecCtx<Mem=M>) {
    match inst {
        &Inst::Nop => exec_nop(ctx),
        _ => unimplemented!(),
    }
}

fn exec_nop<M: Memory>(ctx: &mut ExecCtx<Mem=M>) {
    ctx.regs().pc += 1;
}

#[cfg(test)]
mod test {

    use cpu::reg::Regs;
    use inst::{Inst};
    use mem::*;

    use super::*;

    #[test]
    fn should_exec_nop() {
        let mut ctx = TestCtx::new();
        exec(&Inst::Nop, &mut ctx);
        assert_eq!(ctx.regs.pc, 1);
    }

    struct TestCtx { mem: RamPage, regs: Regs, }

    impl TestCtx {
        fn new() -> Self { TestCtx { mem: RamPage::new(), regs: Regs::new() }}
    }

    impl ExecCtx for TestCtx {
        type Mem = RamPage;
        fn mem(&mut self) -> &mut RamPage { &mut self.mem }
        fn regs(&mut self) -> &mut Regs { &mut self.regs }
    }
}
