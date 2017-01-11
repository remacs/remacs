use std::ptr;
use std::mem;

use libc;

use lisp::{CHECK_TYPE, LispObject, LispType, Qnil, XTYPE, XUNTAG};
use remacs_sys::{Lisp_Object, globals, EmacsInt, wrong_type_argument};

extern "C" {
    static Qconsp: Lisp_Object;
    fn CHECK_IMPURE(obj: Lisp_Object, ptr: *const libc::c_void);
    static Qlistp: Lisp_Object;
}


pub fn CONSP(x: LispObject) -> bool {
    XTYPE(x) == LispType::Lisp_Cons
}

fn atom(object: LispObject) -> LispObject {
    if CONSP(object) {
        Qnil
    } else {
        LispObject::constant_t()
    }
}

defun!("atom",
       Fatom(object),
       Satom,
       atom,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is not a cons cell.  This includes nil.");

fn consp(object: LispObject) -> LispObject {
    if CONSP(object) {
        LispObject::constant_t()
    } else {
        Qnil
    }
}

defun!("consp",
       Fconsp(object),
       Sconsp,
       consp,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is a cons cell.

(fn OBJECT)");

/// Represents a cons cell, or GC bookkeeping for cons cells.
///
/// A cons cell is pair of two pointers, used to build linked lists in
/// lisp.
///
/// Emacs stores available cons cells in a chain. For cells that are
/// free, `car` contains `Vdead` and `cdr` points to the next free
/// cell in the chain.
///
/// # C Porting Notes
///
/// The equivalent C struct is `Lisp_Cons`. Note that the second field
/// may be used as the cdr or GC bookkeeping.
// TODO: this should be aligned to 8 bytes.
#[repr(C)]
#[allow(unused_variables)]
struct LispCons {
    /// Car of this cons cell.
    car: LispObject,
    /// Cdr of this cons cell, or the chain used for the free list.
    cdr: LispObject,
}

/// Extract the LispCons data from an elisp value.
fn XCONS(a: LispObject) -> *mut LispCons {
    debug_assert!(CONSP(a));
    unsafe { mem::transmute(XUNTAG(a, LispType::Lisp_Cons)) }
}

/// Set the car of a cons cell.
fn XSETCAR(c: LispObject, n: LispObject) {
    let cons_cell = XCONS(c);
    unsafe {
        (*cons_cell).car = n;
    }
}

/// Set the cdr of a cons cell.
fn XSETCDR(c: LispObject, n: LispObject) {
    let cons_cell = XCONS(c);
    unsafe {
        (*cons_cell).cdr = n;
    }
}

/// Create a LispObject that's a tagged pointer to this cons cell
/// pointer.
unsafe fn XSETCONS(ptr: *mut libc::c_void) -> LispObject {
    make_lisp_ptr(ptr, LispType::Lisp_Cons)
}

pub fn setcar(cell: LispObject, newcar: LispObject) -> LispObject {
    unsafe {
        CHECK_TYPE(CONSP(cell), LispObject::from_raw(Qconsp), cell);
        CHECK_IMPURE(cell.to_raw(), XCONS(cell) as *const libc::c_void);
    }

    XSETCAR(cell, newcar);
    newcar
}

defun!("setcar",
       Fsetcar(cell, newcar),
       Ssetcar,
       setcar,
       2,
       2,
       ptr::null(),
       "Set the car of CELL to be NEWCAR. Returns NEWCAR.

(fn CELL NEWCAR)");

fn setcdr(cell: LispObject, newcar: LispObject) -> LispObject {
    unsafe {
        CHECK_TYPE(CONSP(cell), LispObject::from_raw(Qconsp), cell);
        CHECK_IMPURE(cell.to_raw(), XCONS(cell) as *const libc::c_void);
    }

    XSETCDR(cell, newcar);
    newcar
}

defun!("setcdr",
       Fsetcdr(cell, newcar),
       Ssetcdr,
       setcdr,
       2,
       2,
       ptr::null(),
       "Set the cdr of CELL to be NEWCDR.  Returns NEWCDR.

(fn CELL NEWCDR)");

/// Is `object` nil?
pub fn NILP(object: LispObject) -> bool {
    object == Qnil
}

unsafe fn XCAR(object: LispObject) -> LispObject {
    (*XCONS(object)).car
}

unsafe fn XCDR(object: LispObject) -> LispObject {
    (*XCONS(object)).cdr
}

/// Take the car/cdr of a cons cell, or signal an error if it's a
/// different type.
///
/// # Porting Notes
///
/// This is equivalent to `CAR`/`CDR` in C code.
fn car(object: LispObject) -> LispObject {
    if CONSP(object) {
        unsafe { XCAR(object) }
    } else if NILP(object) {
        Qnil
    } else {
        LispObject::from_raw(unsafe { wrong_type_argument(Qlistp, object.to_raw()) })
    }
}

defun!("car",
       Fcar(list),
       Scar,
       car,
       1,
       1,
       ptr::null(),
       "Return the car of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a \
        cons cell.  See also `car-safe'.

See Info node `(elisp)Cons Cells' for a discussion of \
        related basic
Lisp concepts such as car, cdr, cons cell and list.

(fn LIST)");

fn cdr(object: LispObject) -> LispObject {
    if CONSP(object) {
        unsafe { XCDR(object) }
    } else if NILP(object) {
        Qnil
    } else {
        LispObject::from_raw(unsafe { wrong_type_argument(Qlistp, object.to_raw()) })
    }
}

defun!("cdr",
       Fcdr(list),
       Scdr,
       cdr,
       1,
       1,
       ptr::null(),
       "Return the cdr of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a \
        cons cell.  See also `cdr-safe'.

See Info node `(elisp)Cons Cells' for a discussion of \
        related basic
Lisp concepts such as cdr, car, cons cell and list.

(fn LIST)");

fn listp(object: LispObject) -> LispObject {
    if CONSP(object) || NILP(object) {
        LispObject::constant_t()
    } else {
        Qnil
    }
}

defun!("listp",
       Flistp(object),
       Slistp,
       listp,
       1,
       1,
       ptr::null(),
       "return t if OBJECT is a list, that is a cons cell or nil, Otherwise, return nil.

(fn \
        OBJECT)");

fn nlistp(object: LispObject) -> LispObject {
    if CONSP(object) || NILP(object) {
        Qnil
    } else {
        LispObject::constant_t()
    }
}

defun!("nlistp",
       Fnlistp(object),
       Snlistp,
       nlistp,
       1,
       1,
       ptr::null(),
       "Return t if OBJECT is not a list.  Lists include nil.

(fn OBJECT)");

// When scanning the C stack for live Lisp objects, Emacs keeps track of
// what memory allocated via lisp_malloc and lisp_align_malloc is intended
// for what purpose.  This enumeration specifies the type of memory.
//
// # Porting Notes
//
// `mem_type` in C.
#[repr(u8)]
#[allow(dead_code)]
#[allow(non_camel_case_types)]
enum MemType {
    MEM_TYPE_NON_LISP,
    MEM_TYPE_BUFFER,
    MEM_TYPE_CONS,
    MEM_TYPE_STRING,
    MEM_TYPE_MISC,
    MEM_TYPE_SYMBOL,
    MEM_TYPE_FLOAT,
    // Since all non-bool pseudovectors are small enough to be
    // allocated from vector blocks, this memory type denotes
    // large regular vectors and large bool pseudovectors.
    MEM_TYPE_VECTORLIKE,
    // Special type to denote vector blocks.
    MEM_TYPE_VECTOR_BLOCK,
    // Special type to denote reserved memory.
    MEM_TYPE_SPARE,
}

extern "C" {
    /// Construct a LispObject from a value or address.
    ///
    /// # Porting Notes
    ///
    /// This function also replaces the C macros `XSETCONS`,
    /// `XSETVECTOR`, `XSETSTRING`, `XSETFLOAT` and `XSETMISC`.
    fn make_lisp_ptr(ptr: *mut libc::c_void, ty: LispType) -> LispObject;
    fn lisp_align_malloc(nbytes: libc::size_t, ty: MemType) -> *mut libc::c_void;
    /// Free-list of Lisp_Cons structures.
    static mut cons_free_list: *mut LispCons;
    static mut consing_since_gc: EmacsInt;
    static mut total_free_conses: EmacsInt;
    // Current cons_block.
    static mut cons_block: *mut ConsBlock;
    // Index of first unused Lisp_Cons in the current block.
    static mut cons_block_index: libc::c_int;
}

const BLOCK_PADDING: usize = 0;
const BLOCK_ALIGN: usize = 1 << 10;

#[cfg(target_pointer_width = "32")]
const SIZE_OF_PTR: usize = 4;
#[cfg(target_pointer_width = "64")]
const SIZE_OF_PTR: usize = 8;

/// We can't call size_of at compile time, so we write a test to
/// verify that the constant has the value we want.
#[test]
fn test_size_of_ptr() {
    assert_eq!(mem::size_of::<*const u32>(), SIZE_OF_PTR);
}

const BLOCK_BYTES: usize = BLOCK_ALIGN - SIZE_OF_PTR - BLOCK_PADDING;

#[cfg(target_pointer_width = "32")]
const SIZE_OF_LISP_CONS: usize = 2 * 4;
#[cfg(all(not(windows), target_pointer_width = "64"))]
const SIZE_OF_LISP_CONS: usize = 2 * 8;
// An EmacsInt is a c_long, which is 32 bits on 64-bit windows:
// http://stackoverflow.com/a/589685/509706
#[cfg(all(windows, target_pointer_width = "64"))]
const SIZE_OF_LISP_CONS: usize = 2 * 4;

/// We can't call size_of at compile time, so we write a test to
/// verify that the constant matches the size of `LispCons`.
#[test]
fn test_size_of_lisp_cons() {
    assert_eq!(mem::size_of::<LispCons>(), SIZE_OF_LISP_CONS);
}

/// An unsigned integer type representing a fixed-length bit sequence,
/// suitable for bool vector words, GC mark bits, etc.
#[allow(non_camel_case_types)]
type bits_word = libc::size_t;

#[cfg(target_pointer_width = "32")]
const SIZE_OF_BITS_WORD: usize = 4;
#[cfg(target_pointer_width = "64")]
const SIZE_OF_BITS_WORD: usize = 8;

#[test]
fn test_size_of_bits_word() {
    assert_eq!(mem::size_of::<bits_word>(), SIZE_OF_BITS_WORD);
}

const CHAR_BIT: usize = 8;

const CONS_BLOCK_SIZE: usize =
    ((BLOCK_BYTES - SIZE_OF_PTR - (SIZE_OF_LISP_CONS - SIZE_OF_BITS_WORD)) * CHAR_BIT) /
    (SIZE_OF_LISP_CONS * CHAR_BIT + 1);

const BITS_PER_BITS_WORD: usize = 8 * SIZE_OF_BITS_WORD;

/// The ConsBlock is used to store cons cells.
///
/// We allocate new ConsBlock values when needed. Cons cells reclaimed
/// by GC are put on a free list to be reallocated before allocating
/// any new cons cells from the latest ConsBlock.
///
/// # Porting Notes
///
/// This is `cons_block` in C.
#[repr(C)]
struct ConsBlock {
    conses: [LispCons; CONS_BLOCK_SIZE as usize],
    gcmarkbits: [bits_word; (1 + CONS_BLOCK_SIZE / BITS_PER_BITS_WORD) as usize],
    next: *mut ConsBlock,
}

fn get_mark_bit(block: *const ConsBlock, n: usize) -> usize {
    unsafe {
        (*block).gcmarkbits[n / BITS_PER_BITS_WORD] >> (n % BITS_PER_BITS_WORD) & 1
    }
}

/// Get pointer to block that contains this cons cell.
fn CONS_BLOCK(fptr: *const LispCons) -> *const ConsBlock {
    (fptr as usize & !(BLOCK_ALIGN - 1)) as *const ConsBlock
}

/// Find the offset of this cons cell in its current block.
fn CONS_INDEX(fptr: *const LispCons) -> usize {
    (fptr as usize & (BLOCK_ALIGN - 1)) / SIZE_OF_LISP_CONS
}

fn cons_marked_p(fptr: *const LispCons) -> bool {
    get_mark_bit(CONS_BLOCK(fptr), CONS_INDEX(fptr)) != 0
}

fn cons(car: LispObject, cdr: LispObject) -> LispObject {
    let val: LispObject;
    unsafe {
        if !cons_free_list.is_null() {
            // Use the current head of the free list for this cons
            // cell, and remove it from the free list.
            val = XSETCONS(cons_free_list as *mut libc::c_void);
            let new_list_head = (*cons_free_list).cdr;
            cons_free_list = mem::transmute(new_list_head);
        } else {
            // Otherwise, we need to malloc some memory.
            if cons_block_index == (CONS_BLOCK_SIZE as libc::c_int) {
                // Allocate a new block.
                let new: *mut ConsBlock = lisp_align_malloc(mem::size_of::<*mut ConsBlock>(),
                                                            MemType::MEM_TYPE_CONS) as
                                          *mut ConsBlock;
                // Set all the cons cells as free.
                libc::memset(&mut (*new).gcmarkbits as *mut _ as *mut libc::c_void,
                             0,
                             mem::size_of_val(&(*new).gcmarkbits));
                // Add the block to the linked list.
                (*new).next = cons_block;
                cons_block = new;
                cons_block_index = 0;
                total_free_conses += CONS_BLOCK_SIZE as EmacsInt;
            }

            let new_cons_cell_ptr = &mut (*cons_block).conses[cons_block_index as usize] as
                                    *mut LispCons;
            val = XSETCONS(new_cons_cell_ptr as *mut libc::c_void);
            cons_block_index += 1;
        }
    }

    XSETCAR(val, car);
    XSETCDR(val, cdr);

    debug_assert!(!cons_marked_p(XCONS(val)));

    unsafe {
        consing_since_gc += mem::size_of::<LispCons>() as EmacsInt;
        total_free_conses -= 1;
        globals.f_cons_cells_consed += 1;
    }

    val
}

defun!("rust-cons",
       Fcons(car, cdr),
       Scons,
       cons,
       2,
       2,
       ptr::null(),
       "Create a new cons, give it CAR and CDR as components, and return it.

(fn CAR CDR)");
