Assignment Description:  
Write a device driver for a character device which implements a simple way of message passing. The kernel maintains a list of messages. To limit memory usage, we impose a limit of 6KB = 6*1024 bytes for each message, and also impose a limit of the size of all messages, which is initially 4MB = 4*1024*1024 bytes.  
  
Your device driver should perform the following operations:  
Creating the device, which has to be /dev/opsysmem, creates an empty list of messages.  
Removing the device deallocates all messages and removes the list of messages.  
Reading from the device returns one message, and removes this message from the kernel list. If the list of messages is empty, the reader returns -EAGAIN.  
Writing to the device stores the message in kernel space and adds it to the list if the message is below the maximum size, and the limit of the size of all messages wouldn't be surpassed with this message. If the message is too big, -EINVAL is returned, and if the limit of the size of all messages was surpassed, -EAGAIN is returned.  The kernel module which implements this driver must be called charDeviceDriver.ko.  
You need to ensure that your code deals with multiple attempts at reading and writing at the same time. The reader should obtain the messages a LIFO (last in first out) manner.  