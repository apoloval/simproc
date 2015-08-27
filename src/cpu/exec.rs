//
// SimProc library
// Copyright (c) 2015 Alvaro Polo
//
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

use cpu::clock::Cycle;
use cpu::reg::Regs;
use inst::{Inst, RuntimeInst, Reg};
use mem::Memory;

pub trait ExecCtx {
    type Mem: Memory;
    fn mem(&mut self) -> &mut Self::Mem;
    fn regs(&mut self) -> &mut Regs;
}

/// Execute the given instruction over the given context
/// It returns the number of cycles that correspond to that instruction execution.
pub fn exec<M: Memory>(inst: &RuntimeInst, ctx: &mut ExecCtx<Mem=M>) -> Cycle {
    match inst {
        &Inst::Nop => exec_nop(ctx),
        &Inst::Add(dst, src) => exec_add(ctx, &dst, &src, false),
        _ => unimplemented!(),
    }
}

fn exec_nop<M: Memory>(ctx: &mut ExecCtx<Mem=M>) -> Cycle {
    ctx.regs().pc += 1;
    4
}

fn exec_add<M: Memory>(ctx: &mut ExecCtx<Mem=M>, dst: &Reg, src: &Reg, carry: bool) -> Cycle {
    ctx.regs().pc += 1;
    4
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
