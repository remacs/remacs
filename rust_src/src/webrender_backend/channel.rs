use std::{
    ffi::CString,
    sync::mpsc::{channel, Receiver, SendError, Sender, TryIter},
};

pub struct SelectableSender<T> {
    inner_sender: Sender<T>,
    raw_fd: i32,
}

impl<T> SelectableSender<T> {
    pub fn new(inner_sender: Sender<T>, raw_fd: i32) -> Self {
        Self {
            inner_sender,
            raw_fd,
        }
    }

    pub fn send(&self, message: T) -> Result<(), SendError<T>> {
        let ret = self.inner_sender.send(message);

        // make raw_fd readable
        unsafe {
            libc::write(
                self.raw_fd,
                CString::new("0").unwrap().as_ptr() as *const libc::c_void,
                2,
            )
        };

        ret
    }
}

pub struct SelectableReceiver<T> {
    inner_receiver: Receiver<T>,
    raw_fd: i32,
}

impl<T> SelectableReceiver<T> {
    pub fn new(inner_receiver: Receiver<T>, raw_fd: i32) -> Self {
        Self {
            inner_receiver,
            raw_fd,
        }
    }

    pub fn get_raw_fd(&self) -> i32 {
        self.raw_fd
    }

    pub fn poll(&self) -> TryIter<T> {
        let mut buffer: [i32; 10] = Default::default();

        // swallow reaable data on raw_fd
        let _ = unsafe {
            libc::read(
                self.raw_fd,
                &mut buffer[0] as *mut _ as *mut libc::c_void,
                10,
            )
        };

        self.inner_receiver.try_iter()
    }
}

pub fn selectable_channel<T>() -> (SelectableSender<T>, SelectableReceiver<T>) {
    let (event_tx, event_rx) = channel::<T>();

    let mut pipes: [i32; 2] = [-1, -1];
    unsafe {
        libc::pipe(&mut pipes[0]);
        libc::fcntl(pipes[0], libc::F_SETFL, libc::O_NONBLOCK);
        libc::fcntl(pipes[1], libc::F_SETFL, libc::O_NONBLOCK)
    };

    let receiver = SelectableReceiver::<T>::new(event_rx, pipes[0]);
    let sender = SelectableSender::<T>::new(event_tx, pipes[1]);

    (sender, receiver)
}
