#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/fs.h>
#include <linux/spinlock.h>
#include <asm/uaccess.h>
#include <linux/slab.h>
#include <linux/sched.h>
#include "charDeviceDriver.h"

MODULE_LICENSE("GPL");

DEFINE_MUTEX(devLock);
DEFINE_MUTEX(rw_lock);
static int counter = 0;

int init_module(void)
{
	Major = register_chrdev(0, DEVICE_NAME, &fops);

	if (Major < 0) {
		printk(KERN_ALERT "Registering char device failed with %d\n", Major);
		return Major;
	}

	msg_ls.head = NULL;
	msg_ls.total_sz = 0;

	printk(KERN_INFO "I was assigned major number %d. To talk to\n", Major);
	printk(KERN_INFO "the driver, create a dev file with\n");
	printk(KERN_INFO "'mknod /dev/%s c %d 0'.\n", DEVICE_NAME, Major);
	printk(KERN_INFO "Try various minor numbers.Try to cat and echo to\n");
	printk(KERN_INFO "the device file.\n");
	printk(KERN_INFO "Remove the device file and module when down.\n");

	return SUCCESS;
}

/*
 * This function is called when the module is unloaded
 */
void cleanup_module(void)
{
	// Unregister the device and clean linked list
	unregister_chrdev(Major, DEVICE_NAME);
	ls_destroy(&msg_ls);
}

/*
 * Methods
 */

/* 
 * Called when a process tries to open the device file, like
 * "cat /dev/mycharfile"
 */
static int
device_open(struct inode *inode, struct file *file)
{
	mutex_lock (&devLock);
	if(Device_Open){
	mutex_unlock(&devLock);
	return -EBUSY;
	}
	Device_Open++;
	mutex_unlock (&devLock);
	sprintf(msg, "I already told you %d times Hello World!\n", counter++);
	msg_Ptr = msg;
	try_module_get(THIS_MODULE);

	return SUCCESS;
}

/* Called when a process closes the device file. */
static int
device_release(struct inode *inode, struct file *file)
{
	mutex_lock (&devLock);
	Device_Open--;
	mutex_unlock(&devLock);
	module_put(THIS_MODULE);

	return SUCCESS;
}

/* 
 * Called when a process, which already opened the dev file, attempts to
 * read from it.
 * This is just an example about how to copy a message in the user space
 * You will need to modify this function
 */
static ssize_t
device_read(struct file *filp, char __user *buffer, size_t length, loff_t *offset)
{
	int bytes_read = 0;

	mutex_lock(&rw_lock);
	if (!ls_remove(&msg_ls, &msg_Ptr)) {
		mutex_unlock(&rw_lock);
		return -EAGAIN;
	}

	if (*msg_Ptr == 0) {
		mutex_unlock(&rw_lock);
		return 0;
	}

	int result;

	while (length && *msg_Ptr) {
		result = put_user(*(msg_Ptr++), buffer++);
		if (result !=0){
			return EFAULT;
		}

		length--;
		bytes_read++;
	}

	if (*msg_Ptr == '\0')
		put_user(*msg_Ptr, buffer);

	kfree(msg_Ptr);
	msg_Ptr = NULL;
	mutex_unlock(&rw_lock);

	return bytes_read;
}

/* Called when a process writes to dev file: echo "hi" > /dev/hello  */
static ssize_t
device_write(struct file *filp, const char __user *buff, size_t len, loff_t *off)
{
	int len1 = len + 1;
	size_t msg1 = len1 * sizeof(char);
	char *buf = NULL;

	if (msg1 > max_msg_len) {
		printk(KERN_ALERT "Sorry,this operation isn't supported.\n");
		return -EINVAL;
	}

	if (msg_ls.total_sz + msg1 > max_msg_ls_len) {
		printk(KERN_ALERT "Error: oversized messages.\n");
		return -EAGAIN;
	}

	// Get string from user space
	mutex_lock(&rw_lock);
	buf = kmalloc(sizeof(char) * len1, GFP_KERNEL);
	strncpy_from_user(buf, buff, len1 - 1);
	buf[len1 - 1] = '\0';

	if (!ls_append(&msg_ls, buf, len1))
		printk(KERN_ALERT "An error occurred when adding the message\n");

	kfree(buf);
	mutex_unlock(&rw_lock);

	return len;
}

int
ls_append(struct linked_list *ls, char *buf, size_t len)
{
	struct node *cur = ls->head;
	if (!cur) {
		ls->head = kmalloc(sizeof(struct node), GFP_KERNEL);
		if (!ls->head)
			return 0;

		ls->head->msg = kmalloc(sizeof(char) * len, GFP_KERNEL);
		strcpy(ls->head->msg, buf);
		ls->head->next = NULL;
	} else {
		while (cur->next)
			cur = cur->next;

		cur->next = kmalloc(sizeof(struct node), GFP_KERNEL);
		if (!cur->next)
			return 0;
		cur->next->msg = kmalloc(sizeof(char) * len, GFP_KERNEL);
		strcpy(cur->next->msg, buf);
		cur->next->next = NULL;
	}

	ls->total_sz += len * sizeof(char);

	return 1;
}

int
ls_remove(struct linked_list *ls, char **buf)
{
	struct node *next_node = NULL;
    size_t msg_sz = 0;

	if (ls->head == NULL) {
        return 0;
    }

	next_node = ls->head->next;

	msg_sz = (strlen(ls->head->msg) + 1) * sizeof(char);

	*buf = kmalloc(msg_sz, GFP_KERNEL);
	strcpy(*buf, ls->head->msg);

	ls->total_sz -= msg_sz;

	kfree(ls->head->msg);
	kfree(ls->head);
	ls->head = next_node;

	return 1;
}

int
ls_destroy(struct linked_list *ls)
{
	struct node *cur = ls->head;
	struct node* tmp;

	while (cur != NULL) {
		tmp = cur;
		cur = cur->next;
		kfree(tmp);
	}

	ls->head = NULL;
	ls->total_sz = 0;

	return 1;
}

