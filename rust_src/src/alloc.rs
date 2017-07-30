use lisp::ExternalPtr;
use hashtable::LispHashTable;
use std::sync::Mutex;
use libc::c_void;

pub trait GCObject: Send + Sync {
    fn mark(&mut self);
    fn unmark(&mut self);
    fn is_marked(&self) -> bool;
}

static BLOCK_SIZE: usize = 4096;

struct Block<T: GCObject + Sized> {
    managed: Vec<Option<T>>,
    free_list: Vec<usize>
}

impl<T: GCObject + Sized> Block<T> {
    fn new() -> Block<T> {
        Block {
            managed: Vec::with_capacity(BLOCK_SIZE),
            free_list: Vec::with_capacity(BLOCK_SIZE)
        }
    }

    fn dealloc(&mut self, idx: usize) {
        assert!(self.managed.len() > idx);
        self.managed[idx] = None;
        self.free_list.push(idx);
    }

    fn alloc(&mut self, t: T) -> ExternalPtr<T> {
        let addr = match self.free_list.pop() {
            Some(idx) => {
                self.managed[idx] = Some(t);
                &mut self.managed[idx]
            },
            
            None => {
                self.managed.push(Some(t));
                let len = self.managed.len();
                &mut self.managed[len - 1]
            }
            
        };

        ExternalPtr::new(addr.as_mut().unwrap())
    }

    fn sweep(&mut self) -> bool {
        let len = self.managed.len();
        let mut all_none = true;
        for i in 0..len {
            if self.managed[i].is_some() {
                if self.managed[i].as_ref().unwrap().is_marked() {
                    self.managed[i].as_mut().unwrap().unmark();
                    all_none = false;
                } else {
                    self.dealloc(i);
                }
            }
        }
    
        all_none
    }
}

struct BlockAllocator<T: GCObject + Sized> {
    blocks: Vec<Box<Block<T>>>,
    curr_block: usize
}

impl<T: GCObject + Sized> BlockAllocator<T> {
    fn new() -> BlockAllocator<T> {
        BlockAllocator {
            blocks: vec![Box::new(Block::new())],
            curr_block: 0
        }
    }

    fn new_block(&mut self, t: T) -> ExternalPtr<T> {
        let mut new_block = Box::new(Block::new());
        let ptr = new_block.alloc(t);
        self.blocks.push(new_block);
        ptr
    }

    fn alloc(&mut self, t: T) -> ExternalPtr<T> {
        if self.blocks.is_empty() {
            self.curr_block = 0;
            return self.new_block(t);
        }
        
        let curr_block = self.curr_block;
        let managed_len = self.blocks[curr_block].managed.len();
        let free_is_empty = self.blocks[curr_block].free_list.is_empty();
        let ptr;
        
        if managed_len == BLOCK_SIZE && free_is_empty {
            ptr = self.new_block(t);
            self.curr_block += 1;
        } else {
            ptr = self.blocks[curr_block].alloc(t);
        }

        ptr
    }

    fn sweep(&mut self) {
        let len = self.blocks.len();
        let mut del = 0;
        {            
            for i in 0..len {
                let empty = self.blocks[i].sweep();
                if empty {
                    del += 1;
                } else if del > 0 {
                    self.blocks.swap(i - del, i);
                }
            }
        }
        
        if del > 0 {
            self.blocks.truncate(len - del);
        }

        self.curr_block = 0;
    }
}

pub struct LispGarbageCollector {
    managed_hashtables: BlockAllocator<LispHashTable>
}

lazy_static! {
    pub static ref GC: Mutex<LispGarbageCollector> = {
        Mutex::new(LispGarbageCollector::new())
    };
}

macro_rules! garbage_collector {
    () => { ::alloc::GC.lock().unwrap() }
}

impl LispGarbageCollector {
    pub fn new() -> LispGarbageCollector {
        LispGarbageCollector {
            managed_hashtables: BlockAllocator::new()
        }
    }
    pub fn manage_hashtable(&mut self, table: LispHashTable) -> ExternalPtr<LispHashTable> {
        // @TODO have equiv of
        // consing_since_gc += nbytes;
        // vector_sells_consed += len;
        self.managed_hashtables.alloc(table)
    }

    pub fn sweep(&mut self) {
        self.managed_hashtables.sweep();
    }
}

// Since we have already done a typecheck on the C layer, we can avoid a vtable look up here
// by casting to a specific type and calling mark directly. In the future, as we transition away from the C,
// we can rely on the trait to do what we need to do.
#[no_mangle]
pub unsafe fn rust_mark_hashtable(ptr: *mut c_void) {
    let mut ptr = ExternalPtr::new(ptr as *mut LispHashTable);
    ptr.mark();
}

#[no_mangle]
pub fn rust_sweep() {
    garbage_collector!().sweep();
}

#[cfg(test)]
macro_rules! count_managed {
    ($gc: ident, $map: ident) => ({
        let mut count = 0;
        for x in $gc.$map.blocks.iter() {
            let countvec = x.managed.iter()
                .filter(|x| x.is_some())
                .collect::<Vec<_>>();
            count += countvec.len();
        }

        count
    })
}

#[test]
fn gc_collection() {
    let mut gc = LispGarbageCollector::new();
    let mut table = gc.manage_hashtable(LispHashTable::new());
    table.mark();
    gc.sweep();
    assert!(count_managed!(gc, managed_hashtables) == 1);

    gc.sweep();
    assert!(count_managed!(gc, managed_hashtables) == 0);
}

#[test]
fn gc_ptr_management() {
    let mut gc = LispGarbageCollector::new();
    let mut table = gc.manage_hashtable(LispHashTable::new());
    table.mark();
    assert!(table.is_marked());
    table.unmark();
    assert!(!table.is_marked());
}

#[test]
fn gc_collection_2() {
    let mut gc = LispGarbageCollector::new();
    let mut table = gc.manage_hashtable(LispHashTable::new());
    let mut table2 = gc.manage_hashtable(LispHashTable::new());
    let mut table3 = gc.manage_hashtable(LispHashTable::new());
    table.mark();
    table2.mark();
    table3.mark();
    gc.sweep();
    assert!(count_managed!(gc, managed_hashtables) == 3);
    
    table.mark();
    table2.mark();
    gc.sweep();
    assert!(count_managed!(gc, managed_hashtables) == 2);

    gc.sweep();
    assert!(count_managed!(gc, managed_hashtables) == 0);
}

#[test]
fn gc_collection_3() {
    let mut gc = LispGarbageCollector::new();
    for idx in 0..1024 {
        let mut table = gc.manage_hashtable(LispHashTable::new());
        if idx % 2 == 0 {
            table.mark();
        }
    }

    gc.sweep();
    assert!(count_managed!(gc, managed_hashtables) == 512);
}

#[test]
fn block_alloc_test() {
    let mut gc = LispGarbageCollector::new();
    let mut vec = Vec::new();
    let count = BLOCK_SIZE + (BLOCK_SIZE / 2);
    for _ in 0..count {
        vec.push(gc.manage_hashtable(LispHashTable::new()));
    }

    assert!(gc.managed_hashtables.blocks.len() == 2);

    for idx in 0..BLOCK_SIZE {
        vec[idx].mark();
    }

    gc.sweep();
    assert!(gc.managed_hashtables.blocks.len() == 1);

    gc.sweep();
    assert!(gc.managed_hashtables.blocks.len() == 0);
    gc.manage_hashtable(LispHashTable::new());
    assert!(gc.managed_hashtables.blocks.len() == 1);
}
