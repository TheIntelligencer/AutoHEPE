//**********************************************************
//     ***   ***  ********* *********  *********
//     ***   ***  ***       ***   ***  ***
//     *********  ********* ***   ***  *********
//     *********  ********* *** *****  *********
//     ***   ***  ***       ***        ***
//     ***   ***  ********* ***        *********
//
//     AUTOHEPE.exe - autocheck HEPE realization
//      
//         Copyright 2022(C) Mikhail Gurin
//
//              Module name: main.c
//
//    This module contains main logic of AUTOHEPE
//
//                  08/05/2022
//
//**********************************************************

#define _NO_NTDLLP_WSTRFUNCS_
#include <ntdll.h>

#pragma warning(disable: 4996)
#pragma warning(disable: 4244)

#define HEPE_VER L"1.3"

#define SESSION_MANAGER_KEY L"\\REGISTRY\\MACHINE\\SYSTEM\\CurrentControlSet\\Control\\Session Manager"
#define BOOT_EXECUTE_KEY L"BootExecute"
#define AUTOSTART_VALUE L"autocheck autochk *"

#define HEPE_MARKER L"autohepe.cf"
#define HEPE_OUTPUT_BUFFER_SIZE 2048
#define HEPE_HELLO_MSG1 L"Hard disk eraser - ver. 1.3\r\n"
#define HEPE_HELLO_MSG2 L"Made by Mikhail Gurin, 2022\r\n"
#define HEPE_HELLO_MSG3 L"Selected drive:"
#define HEPE_HELLO_MSG4 L"Erasing block size:"

#define HEPE_OPERATION_CONFIRMED L"\rOperation confirmed.                         \r\n\r\n\r\n"
#define HEPE_OPERATION_CANCELLED L"\rOperation cancelled.                         \r\n"
#define HEPE_OPERATION_COMPLETED L"\r\n\rOperation completed.                         \r\n\r\n"
#define HEPE_OPERATION_NOTCOMPLT L"\rOperation NOT completed due errors! =(       \r\n\r\n"

#define HEPE_OSRESTART_MSG L"\r\nWindows will restarted now.\r\n"

#define HEPE_DELAY_AFTER_CANCEL -10 * 1000 * 5000
#define HEPE_DELAY_AFTER_SUCCESS -10 * 1000 * 5000

//HEPE Memory managment
#define INVALID_MEMORY_SIZE -1

PVOID ZgGlobalHeap = NULL;

//*********************************************************
//
//   Function: ZgInitHeap
//
//   Accept arguments: void
//
//   Return value: void
//
//   Info: This function provides heap initialization for
//         current process. If resource is insufficient,
//         Windows will crash!
//
//*********************************************************
void ZgInitHeap()
{
    BOOLEAN PrivLastState;

    ZgGlobalHeap = RtlCreateHeap(HEAP_GROWABLE, NULL, 0, 100 * 1024, NULL, NULL);

    if (ZgGlobalHeap == NULL)
    {
        RtlAdjustPrivilege(SE_DEBUG_PRIVILEGE, TRUE, FALSE, &PrivLastState);
        RtlAdjustPrivilege(SE_SHUTDOWN_PRIVILEGE, TRUE, FALSE, &PrivLastState);
        NtRaiseHardError(STATUS_INSUFFICIENT_RESOURCES, 0, 0, NULL, OptionShutdownSystem, NULL);
    }
}

//*********************************************************
//
//   Function: ZgFreeHeap
//
//   Accept arguments: void
//
//   Return value: void
//
//   Info: This function provides heap destoying.
//         Must be called before process shutdown!
//
//*********************************************************
void ZgFreeHeap()
{
    if (ZgGlobalHeap) RtlDestroyHeap(ZgGlobalHeap);
}

//*********************************************************
//
//   Function: ZgMalloc
//
//   Accept arguments: size_t
//
//   Return value: void *
//
//   Info: this function allocates memory on process heap 
//         WARNING: You must call ZgInitHeap before using
//         this function otherwise returned handle will
//          be invalid.
//         if argument equals zero, function'll return 0!
//
//*********************************************************
void * ZgMalloc(SIZE_T size)
{
    if (ZgGlobalHeap == NULL) return NULL;
    return RtlAllocateHeap(ZgGlobalHeap, 0, size);
}

//*********************************************************
//
//   Function: ZgSizeOf
//
//   Accept arguments: void *
//
//   Return value: size_t
//
//   Info: this function get size of block that was
//         allocated on global process heap.
//         Input pointer must be correct pointer that
//         points to global heap memory!
//         if argument equals zero, function'll return 0!
//
//*********************************************************
size_t ZgSizeOf(void *addr)
{
    return (ZgGlobalHeap && addr) ? RtlSizeHeap(ZgGlobalHeap, 0, addr) : INVALID_MEMORY_SIZE;
}

//*********************************************************
//
//   Function: ZgFree
//
//   Accept arguments: void *
//
//   Return value: void
//
//   Info: this function frees memory
//         allocated on global process heap.
//         Input pointer must be correct pointer that
//         points to global heap memory!
//
//*********************************************************
void ZgFree(void *addr)
{
    if (ZgGlobalHeap && addr) RtlFreeHeap(ZgGlobalHeap, 0, addr);
}

//HEPE input managment

#define HEPE_KB_CNT 64
#define HEPE_KB_NAM L"\\Device\\KeyboardClass"
#define HEPE_KB_LEN 32

//*********************************************************
//
//   Function: ZgGetKeyboards
//
//   Accept arguments: keyboards: HANDLE**
//                     actualcount: DWORD*
//
//   Return value: NTSTATUS
//
//   Info: This function builds array of available
//          keyboards and gets them count
//
//*********************************************************
NTSTATUS ZgGetKeyboards(HANDLE **keyboards, DWORD *actualcount)
{
    NTSTATUS            status;
    WCHAR               currentkbd_buf[HEPE_KB_LEN];
    UNICODE_STRING      currentkbd;
    OBJECT_ATTRIBUTES   currentkbd_oa;
    IO_STATUS_BLOCK     currentkbd_isb;
    DWORD               kbdcounter;
    DWORD               i, j;

    //return error status if pointer to actualcount equals zero
    if (actualcount == NULL) return STATUS_INSUFFICIENT_RESOURCES;

    //setting initial value to kbdcounter
    kbdcounter = 0;

    //allocating array of handles
    *keyboards = (PHANDLE)ZgMalloc(HEPE_KB_CNT);

    //can't continue work without allocated kbd array
    if (*keyboards == NULL) return STATUS_INSUFFICIENT_RESOURCES;

    //every keyboard name equals "\\Device\\KeyboardClassX"
    //where X is keyboaard index so we just need to check
    //all of indices from 0 to HEPE_KB_CNT
    for (i = 0, j = 0; i < HEPE_KB_CNT; ++i)
    {
        //preparing keyboard name;
        memset(currentkbd_buf, 0, sizeof(WCHAR) * HEPE_KB_LEN);
        _snwprintf(currentkbd_buf, HEPE_KB_LEN, L"%ws%u\0", HEPE_KB_NAM, i);
        RtlInitUnicodeString(&currentkbd, currentkbd_buf);

        InitializeObjectAttributes(&currentkbd_oa, &currentkbd, OBJ_CASE_INSENSITIVE, NULL, NULL);

        //trying to create keyboard handle
        status = NtCreateFile(
            &((*keyboards)[j]), 
            FILE_READ_ATTRIBUTES | GENERIC_READ | SYNCHRONIZE,
            &currentkbd_oa,
            &currentkbd_isb,
            NULL,
            FILE_ATTRIBUTE_NORMAL,
            FILE_SHARE_READ,
            FILE_OPEN,
            FILE_DIRECTORY_FILE,
            NULL,
            0
        );

        //if handle was created we can increase actual index and count
        if (NT_SUCCESS(status))
        {
            ++j;
            ++kbdcounter;
        }
    }

    //finally we need to return actual keyboards count to argument
    *actualcount = kbdcounter;
    return STATUS_SUCCESS;
}

//*********************************************************
//
//   Function: ZgFreeKeyboards
//
//   Accept arguments: keyboards: HANDLE**
//
//   Return value: NTSTATUS (STATUS_SUCCESS always)
//
//   Info: We must to free earlier allocated keyboards
//          array so this function do this!
//
//*********************************************************
NTSTATUS ZgFreeKeyboards(HANDLE **keyboards, DWORD actualcount)
{
    DWORD i;

    for (i = 0; i < actualcount; ++i)
        NtClose((*keyboards)[i]);

    ZgFree(*keyboards);
    
    return STATUS_SUCCESS;
}

#define HEPE_DELAY_MSG L"To cancel operation press any key in"
#define HEPE_DELAY_SEC -10 * 1000 * 1000
#define HEPE_DELAY_CNT 9

typedef struct _KEYBOARD_INPUT_DATA {
	USHORT UnitId;
	USHORT MakeCode;
	USHORT Flags;
	USHORT Reserved;
	ULONG  ExtraInformation;
} 
KEYBOARD_INPUT_DATA, *PKEYBOARD_INPUT_DATA;

//*********************************************************
//
//   Function: ZgHEPEDelayExecution
//
//   Accept arguments: none
//
//   Return value: NTSTATUS
//
//   Info: We need to give use end user some time (10 sec?)
//      to allow him stop operation before starting
//      because harddisk erasing can't be cancelled
//      even you decide to reset PC during operation;
//      function will return STATUS_SUCCESS if
//      operation is confirmed (not got the signal from all
//      of keyboards) or STATUS_CANCELLED if user pressed
//      any key during delay;
//      This function can return any other NTSTATUS value
//      if an error was occured!
//
//*********************************************************
NTSTATUS ZgHEPEDelayExecution()
{
    NTSTATUS            status;
    PHANDLE             keyboards;
    DWORD               keyboards_count;
    IO_STATUS_BLOCK     keyboards_isb;
    LARGE_INTEGER       keyboards_byteoffset;
    KEYBOARD_INPUT_DATA keyboards_data;
    HANDLE              keyboardevent;
    OBJECT_ATTRIBUTES   keyboardevent_oa;
    UNICODE_STRING      delaymsg;
    WCHAR               delaymsgbuf[HEPE_OUTPUT_BUFFER_SIZE];
    DWORD               delaytimeout;
    LARGE_INTEGER       delaytimeout_c;
    DWORD               i; //counter

    //firstly we need to acquire all available keyboard handles
    if (!NT_SUCCESS(status = ZgGetKeyboards(&keyboards, &keyboards_count)))
    {
        DbgPrint("\r\nAUTOHEPE (delay execution): can't get keyboards array with status: 0x%08x\r\n", status);
        return status;
    }

    //can't continue if no keyboards has been detected;
    if (keyboards_count < 1)
    {
        DbgPrint("\r\nAUTOHEPE (delay execution): seems this computer works without any keyboard, can't continue!\r\n");
        ZgFreeKeyboards(&keyboards, keyboards_count);
        return STATUS_NO_SUCH_DEVICE;
    }

    //now we need to start async reading from all available keyboards
    memset(&keyboards_data, 0, sizeof(KEYBOARD_INPUT_DATA));
    InitializeObjectAttributes(&keyboardevent_oa, NULL, 0, NULL, NULL);
    NtCreateEvent(&keyboardevent, EVENT_ALL_ACCESS, &keyboardevent_oa, SynchronizationEvent, FALSE);

    memset(&keyboards_byteoffset, 0, sizeof(LARGE_INTEGER));
    keyboards_byteoffset.QuadPart = 0;

    for (i = 0; i < keyboards_count; ++i)
    {
        do
        {
            status = NtReadFile(
                keyboards[i],
                keyboardevent,
                NULL,
                NULL,
                &keyboards_isb,
                &keyboards_data,
                sizeof(KEYBOARD_INPUT_DATA),
                &keyboards_byteoffset,
                NULL
             );
            //any other status means that HEPE faced the problem
            if ((status != STATUS_SUCCESS) && (status != STATUS_PENDING)) break;
        } while (status != STATUS_PENDING);
    }

    //return error status if not STATUS_PENDING acquired;
    if (status != STATUS_PENDING)
    {
        DbgPrint("\r\nAUTOHEPE (delay execution): keyboards reading loop finished with status: 0x%08x\r\n", status);
        for (i = 0; i < keyboards_count; ++i) NtCancelIoFile(keyboards[i], &keyboards_isb);
        NtClose(keyboardevent);
        ZgFreeKeyboards(&keyboards, keyboards_count);
        return status;
    }

    //finally starting timeout procedure
    delaytimeout = HEPE_DELAY_CNT;
    memset(&delaytimeout_c, 0, sizeof(LARGE_INTEGER));
    delaytimeout_c.QuadPart = HEPE_DELAY_SEC;

    for (; delaytimeout > 0 ; --delaytimeout)
    {
        memset(delaymsgbuf, 0, HEPE_OUTPUT_BUFFER_SIZE * sizeof(WCHAR));
        _snwprintf(delaymsgbuf, HEPE_OUTPUT_BUFFER_SIZE, L"%\r%ws %u\0", HEPE_DELAY_MSG, delaytimeout);
        RtlInitUnicodeString(&delaymsg, delaymsgbuf);
        NtDisplayString(&delaymsg);

        status = NtWaitForSingleObject(keyboardevent, FALSE, &delaytimeout_c);
        if (status != STATUS_TIMEOUT) break;
    }

    //after all we need to free resources
    for (i = 0; i < keyboards_count; ++i) NtCancelIoFile(keyboards[i], &keyboards_isb);
    NtClose(keyboardevent);
    ZgFreeKeyboards(&keyboards, keyboards_count);

    if (status == STATUS_TIMEOUT)
        status = STATUS_SUCCESS;
    else
        status = STATUS_CANCELLED;

    return status;
}

typedef struct __ZG_HEPE_ARGS {
    WCHAR   drive[MAX_PATH];
    LONG    eraseblocksize;
} ZG_HEPE_ARGS, *PZG_HEPE_ARGS;

//*********************************************************
//
//   Function: ZgParseHEPEArgs
//
//   Accept arguments: argstr: PWCHAR
//                       args: OUT PZG_HEPE_ARGS
//
//   Return value: NTSTATUS
//
//   Info: This function provides startup argument parsing
//          for hepe
//
//*********************************************************
NTSTATUS ZgParseHEPEArgs(PWCHAR argstr, OUT PZG_HEPE_ARGS args)
{
    PWCHAR  argstr_tmp;
    PWCHAR  entry1;
    PWCHAR  entry2;

    if (args == NULL)
        return STATUS_INVALID_PARAMETER_2;

    if (argstr == NULL)
        return STATUS_INVALID_PARAMETER_1;

    argstr_tmp = (PWCHAR)ZgMalloc(wcslen(argstr) * sizeof(WCHAR) + sizeof(WCHAR));
    
    if (argstr_tmp == NULL) return STATUS_INSUFFICIENT_RESOURCES;

    wcscpy(argstr_tmp, argstr);

    entry1 = wcschr(argstr_tmp, L' ');
    if (entry1 == NULL)
    {
        ZgFree(argstr_tmp);
        return STATUS_UNSUCCESSFUL;
    }
    *entry1 = L'\0';

    entry2 = wcschr(entry1 + 1, L' ');
    if (entry2 == NULL)
    {
        ZgFree(argstr_tmp);
        return STATUS_UNSUCCESSFUL;
    }
    *entry2 = L'\0';

    wcscpy(args->drive, entry1 + 1);
    args->eraseblocksize = _wtol(entry2 + 1);

    ZgFree(argstr_tmp);

    return STATUS_SUCCESS;
}

//*********************************************************
//
//   Function: ZgRemoveHEPECF
//
//   Accept arguments: none
//
//   Return value: NTSTATUS
//
//   Info: Removes HEPE marker file
//
//*********************************************************
NTSTATUS ZgRemoveHEPECF()
{
    OBJECT_ATTRIBUTES   fileprobe_oa;
    UNICODE_STRING      fileprobe_string;
    WCHAR               fileprobe_loc[MAX_PATH];

    _snwprintf(fileprobe_loc, MAX_PATH, L"\\??\\%ws\\%ws", GetKUserSharedData()->NtSystemRoot, HEPE_MARKER);
    RtlInitUnicodeString(&fileprobe_string, fileprobe_loc);
    InitializeObjectAttributes(&fileprobe_oa, &fileprobe_string, OBJ_CASE_INSENSITIVE, NULL, NULL);

    return NtDeleteFile(&fileprobe_oa);
}

//*********************************************************
//
//   Function: ZgValidateHEPE
//
//   Accept arguments: none
//
//   Return value: NTSTATUS
//
//   Info: HEPE creates special marker file which let HEPE
//          determine first start or not. If file is not
//          exists it means that HEPE launched first time
//
//*********************************************************
NTSTATUS ZgValidateHEPE()
{
    NTSTATUS            status;
    HANDLE              fileprobe;
    OBJECT_ATTRIBUTES   fileprobe_oa;
    UNICODE_STRING      fileprobe_string;
    IO_STATUS_BLOCK     fileprobe_isb;
    WCHAR               fileprobe_loc[MAX_PATH];

    _snwprintf(fileprobe_loc, MAX_PATH, L"\\??\\%ws\\%ws", GetKUserSharedData()->NtSystemRoot, HEPE_MARKER);

    RtlInitUnicodeString(&fileprobe_string, fileprobe_loc);
    InitializeObjectAttributes(&fileprobe_oa, &fileprobe_string, OBJ_CASE_INSENSITIVE, NULL, NULL);

    status = NtCreateFile(&fileprobe, FILE_READ_ACCESS | FILE_WRITE_ACCESS, &fileprobe_oa, &fileprobe_isb,
        NULL, FILE_ATTRIBUTE_NORMAL, 0, FILE_CREATE, FILE_NO_INTERMEDIATE_BUFFERING | FILE_NON_DIRECTORY_FILE,
        NULL, 0);

    if (!NT_SUCCESS(status))
    {
        if (status == STATUS_OBJECT_NAME_COLLISION)
        {
            DbgPrint("\r\nAUTOHEPE (ZgValidateHEPE): launched second time!\r\n");
            NtDeleteFile(&fileprobe_oa);
            return status;
        }
        else
        {
            DbgPrint("\r\nAUTOHEPE (ZgValidateHEPE): something goes wrong (returned status 0x%08x)!\r\n", status);
            return status;
        }
    }

    //otherwise just flush file buffer to disk
    NtFlushBuffersFile(fileprobe, &fileprobe_isb);
    NtClose(fileprobe);
    return STATUS_SUCCESS;
}

//*********************************************************
//
//   Function: ZgClearAutocheck
//
//   Accept arguments: none
//
//   Return value: NTSTATUS
//
//   Info: reset BootExecute parameter of
//         \\REGISTRY\\MACHINE\\System\\CurrentControlSet\\
//          Control\\SessionManager key.
//
//*********************************************************
NTSTATUS ZgClearAutocheck()
{
    NTSTATUS            status;
    HANDLE              target;
    OBJECT_ATTRIBUTES   target_oa;
    UNICODE_STRING      autostart;
    UNICODE_STRING      sessmgr;
    UNICODE_STRING      bootexecute;

    //Firstly we need to initialize all string with constants
    RtlInitUnicodeString(&sessmgr, SESSION_MANAGER_KEY);
    RtlInitUnicodeString(&bootexecute, BOOT_EXECUTE_KEY);
    RtlInitUnicodeString(&autostart, AUTOSTART_VALUE);

    //Secondly we need to initialize obkect attributes for NtOpenKey
    InitializeObjectAttributes(&target_oa, &sessmgr, OBJ_CASE_INSENSITIVE, NULL, NULL);

    //Now we can change value in registry
    status = NtOpenKey(&target, KEY_ALL_ACCESS, &target_oa);
    if (!NT_SUCCESS(status))
    {
        DbgPrint("\r\nAUTOHEPE (ZgClearAutocheck): can't open registry key with status code 0x%08x\r\n", status);
        return status;
    }

    status = NtSetValueKey(target, &bootexecute, 0, REG_MULTI_SZ, 
		autostart.Buffer, autostart.MaximumLength);

    if (!NT_SUCCESS(status))
	{
        DbgPrint("\r\nAUTOHEPE (ZgClearAutocheck): can't change registry value with status code 0x%08x\r\n", status);
		NtClose(target);
		return status;
	}

    //writing changes to disc
    NtFlushKey(target);
    NtClose(target);
    return status;
}

//handle list definition and routines;
typedef struct __ZG_HANDLE_LIST {
    HANDLE                  handle;
    struct __ZG_HANDLE_LIST *next;
} ZG_HANDLE_LIST, *PZG_HANDLE_LIST;

//*********************************************************
//
//   Function: ZgAddToHandleList
//
//   Accept arguments: PZG_HANDLE_LIST *target
//                     HANDLE handle
//
//   Return value: none
//
//   Info: allocates memory for handle list entry and
//          and places it at the beginning of the list
//          WARNING: ZgInitHeap MUST be called before
//          this calling this function!
//
//*********************************************************
void ZgAddToHandleList(PZG_HANDLE_LIST *target, HANDLE handle)
{
    PZG_HANDLE_LIST tmp;

    //adding first element;
    if (*target == NULL)
    {
        *target = (PZG_HANDLE_LIST)ZgMalloc(sizeof(ZG_HANDLE_LIST));
        (*target)->handle = handle;
        (*target)->next = NULL;
    }
    //adding next elements;
    else
    {
        tmp = *target;
        while (tmp->next != NULL) tmp = tmp->next;
        tmp->next = (PZG_HANDLE_LIST)ZgMalloc(sizeof(ZG_HANDLE_LIST));
        tmp->next->handle = handle;
        tmp->next->next = NULL;
    }
}

//*********************************************************
//
//   Function: ZgCloseAndFreeHandleList
//
//   Accept arguments: PZG_HANDLE_LIST *target
//
//   Return value: none
//
//   Info: closes all handles and frees allocated memory
//
//*********************************************************
void ZgCloseAndFreeHandleList(PZG_HANDLE_LIST *target)
{
    PZG_HANDLE_LIST tmp;

    while (*target != NULL)
    {
        tmp = *target;
        *target = (*target)->next;
        NtClose(tmp->handle);
        ZgFree(tmp);
    }
}

//*********************************************************
//
//   Function: ZgGetStorageDeviceId
//
//   Accept arguments: StorageDevice:   PWCHAR
//                     StorageDeviceId: PLONG
//
//   Return value: NTSTATUS
//
//   Info: retrieves storage device id from string name
//
//*********************************************************
NTSTATUS ZgGetStorageDeviceId(PWCHAR StorageDevice, PLONG StorageDeviceId)
{
    NTSTATUS                status;
    HANDLE                  drive;
    OBJECT_ATTRIBUTES       drive_oa;
    IO_STATUS_BLOCK         drive_isb;
    UNICODE_STRING          drive_name;
    STORAGE_DEVICE_NUMBER   drive_number;

    //can't continue if storage device if is unavaliable
    if (StorageDeviceId == NULL) return STATUS_INVALID_PARAMETER_2;

    RtlInitUnicodeString(&drive_name, StorageDevice);
    InitializeObjectAttributes(&drive_oa, &drive_name, OBJ_CASE_INSENSITIVE, NULL, NULL);

    status = NtOpenFile(
        &drive,
        FILE_ALL_ACCESS,
        &drive_oa,
        &drive_isb,
        FILE_SHARE_WRITE | FILE_SHARE_READ | FILE_SHARE_DELETE,
        0
     );

    if (!NT_SUCCESS(status))
    {
        DbgPrint("\r\nAUTOHEPE (ZgGetStorageDeviceId): can't acquire disk handle with status code 0x%08x\r\n", status);
        return status;
    }

    //required value is placed in DeviceNumber field of STORAGE_DEVICE_NUMBER structure
    memset(&drive_number, 0, sizeof(STORAGE_DEVICE_NUMBER));
    status = NtDeviceIoControlFile(
        drive,
        NULL,
        NULL,
        NULL,
        &drive_isb,
        IOCTL_STORAGE_GET_DEVICE_NUMBER,
        NULL,
        0,
        &drive_number,
        sizeof(STORAGE_DEVICE_NUMBER)
    );

    if (!NT_SUCCESS(status))
    {
        DbgPrint("\r\nAUTOHEPE (ZgGetStorageDeviceId): can't acquire drive device number with status code 0x%08x\r\n", status);
        return status;
    }

    (*StorageDeviceId) = drive_number.DeviceNumber;

    NtClose(drive);
    return STATUS_SUCCESS;
}

//volume blocking
#define HEPE_VOLUME_NAME_BASE       L"\\Device\\HarddiskVolume"
#define HEPE_VOLUME_LOCK_ERR_MSG1   L"\r\nCan't get target drive number with status code 0x%08x\r\n\0"
#define HEPE_VOLUME_COUNT_MAX       128

//*********************************************************
//
//   Function: ZgHEPELockAndDismountVolumes
//
//   Accept arguments: targetdrive:      PWCHAR
//                     lockedhandlelist: PZG_HANDLE_LIST
//
//   Return value: NTSTATUS
//
//   Info: Locks and dismount every volume on target drive
//
//*********************************************************
NTSTATUS ZgHEPELockAndDismountVolumes(
    PWCHAR              targetdrive,
    PZG_HANDLE_LIST     *lockedhandlelist
    )
{
    NTSTATUS            status;
    HANDLE              volume;
    OBJECT_ATTRIBUTES   volume_oa;
    IO_STATUS_BLOCK     volume_isb;
    UNICODE_STRING      volume_name;
    WCHAR               volume_name_buf[MAX_PATH];
    LONG                i; //counter
    LONG                driveid;
    LONG                volume_id;
    UNICODE_STRING      lock_err_msg1;
    WCHAR               lock_err_msg1_buf[HEPE_OUTPUT_BUFFER_SIZE];

    //can't continue without drive id
    driveid = -1;
    if (!NT_SUCCESS(status = ZgGetStorageDeviceId(targetdrive, &driveid)))
    {
        DbgPrint("\r\nAUTOHEPE (ZgHEPELockAndDismountVolumes): can't get target drive number with status code 0x%08x\r\n", status);
        memset(lock_err_msg1_buf, 0, sizeof(WCHAR) * HEPE_OUTPUT_BUFFER_SIZE);
        _snwprintf(lock_err_msg1_buf, HEPE_OUTPUT_BUFFER_SIZE, HEPE_VOLUME_LOCK_ERR_MSG1, status);
        RtlInitUnicodeString(&lock_err_msg1, lock_err_msg1_buf);
        NtDisplayString(&lock_err_msg1);
        return status;
    }

    //numeration starts from 1 to HEPE_VOLUME_COUNT_MAX
    for (i = 1; i <= HEPE_VOLUME_COUNT_MAX; ++i)
    {
        memset(volume_name_buf, 0, sizeof(WCHAR) * MAX_PATH);
        _snwprintf(volume_name_buf, MAX_PATH, L"%ws%ld\0", HEPE_VOLUME_NAME_BASE, i);
        RtlInitUnicodeString(&volume_name, volume_name_buf);
        InitializeObjectAttributes(&volume_oa, &volume_name, 0/*OBJ_CASE_INSENSITIVE*/, NULL, NULL);

        if (NT_SUCCESS(status = NtOpenFile(
                            &volume, 
                            FILE_ALL_ACCESS,
                            &volume_oa,
                            &volume_isb,
                            FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                            0)))
        {
            //trying to get volume device id;
            volume_id = -1;
            status = ZgGetStorageDeviceId(volume_name.Buffer, &volume_id);
            if (!NT_SUCCESS(status))
            {
                DbgPrint("\r\nAUTOHEPE (ZgHEPELockAndDismountVolumes): can't get volume %ws number with status code 0x%08x\r\n",
                    volume_name.Buffer,
                    status);
                NtClose(volume);
                continue;
            }

            if (volume_id != driveid)
            {
                DbgPrint("\r\nAUTOHEPE (ZgHEPELockAndDismountVolumes): volume %ws dev. number not matching arg. dev. number. Skipping...\r\n",
                    volume_name.Buffer);
                NtClose(volume);
                continue;
            }

            //if all is ok we can lock and dismount volume
            status = NtFsControlFile(
                volume,
                NULL,
                NULL,
                NULL,
                &volume_isb,
                FSCTL_LOCK_VOLUME,
                NULL,
                0,
                NULL,
                0
            );

            if (!NT_SUCCESS(status))
            {
                DbgPrint("\r\nAUTOHEPE (ZgHEPELockAndDismountVolumes): can't lock volume %ws with status code 0x%08x\r\n",
                    volume_name.Buffer,
                    status);
                NtClose(volume);
                continue;
            }

            status = NtFsControlFile(
                volume,
                NULL,
                NULL,
                NULL,
                &volume_isb,
                FSCTL_DISMOUNT_VOLUME,
                NULL,
                0,
                NULL,
                0
            );

            if (!NT_SUCCESS(status))
            {
                DbgPrint("\r\nAUTOHEPE (ZgHEPELockAndDismountVolumes): can't dismount volume %ws with status code 0x%08x\r\n",
                    volume_name.Buffer,
                    status);
                NtClose(volume);
                continue;
            }

            ZgAddToHandleList(&(*lockedhandlelist), volume);
        }
    }

    return STATUS_SUCCESS;
}

//erasing operations
#define HEPE_ERASING_TBC_MSG L"Total blocks count is:"
#define HEPE_ERASING_WRN_MSG L"Harddisk is erasing now. Don't turn off your PC!\r\n\r\n"
#define HEPE_ERASING_CUR_MSG L"\rCurrent block is:"
#define HEPE_ERASING_NUL_MSG L"                "
#define HEPE_ERASING_ER2_MSG L"\r\nCan't erase block %I64u with status code: 0x%08x\r\n\0"
#define HEPE_ERASING_ER1_MSG L"\r\nCan't seek to block %I64u with status code: 0x%08x\r\n\0"
#define HEPE_ERASING_ERA_MSG L"\r\nCan't acquire disk handle with status code 0x%08x\r\n\r\n\0"
#define HEPE_ERASING_ERB_MSG L"\r\nCan't acquire disk geometry with status code 0x%08x\r\n\r\n\0"
#define HEPE_ERASING_ERC_MSG L"\r\nErase block size must be multiple hard drive sector size!\r\n\r\n\0"
#define HEPE_ERASING_ERD_MSG L"\r\nCan't acquire disk size with status code 0x%08x\r\n\r\n\0"
#define HEPE_ERASING_ERE_MSG L"\r\nInsufficient resources to allocate erasing block!\r\n\r\n\0"
#define FILE_USE_FILE_POINTER_POSITION 0xFFFFFFFE

//*********************************************************
//
//   Function: ZgHEPEEraseHarddisk
//
//   Accept arguments: DiskName: PWCHAR
//                     EraseBlockSize: ULONG
//
//   Return value: NTSTATUS
//
//   Info: main harddisk erasing routine
//
//*********************************************************
NTSTATUS ZgHEPEEraseHarddisk(PWCHAR DiskName, ULONG EraseBlockSize)
{
    NTSTATUS                    status;
    HANDLE                      disk;
    UNICODE_STRING              disk_name;
    OBJECT_ATTRIBUTES           disk_oa;
    IO_STATUS_BLOCK             disk_isb;
    PVOID                       disk_buf;
    GET_LENGTH_INFORMATION      disk_size;
    LONGLONG                    disk_blocks_count;
    DISK_GEOMETRY               disk_geo;
    UNICODE_STRING              disk_tbc_msg;
    WCHAR                       disk_tbc_msg_buf[HEPE_OUTPUT_BUFFER_SIZE];
    UNICODE_STRING              disk_wrn_msg;
    UNICODE_STRING              disk_cur_msg;
    WCHAR                       disk_cur_msg_buf[HEPE_OUTPUT_BUFFER_SIZE];
    UNICODE_STRING              disk_er1_msg;
    WCHAR                       disk_er1_msg_buf[HEPE_OUTPUT_BUFFER_SIZE];
    UNICODE_STRING              disk_er2_msg;
    WCHAR                       disk_er2_msg_buf[HEPE_OUTPUT_BUFFER_SIZE];
    LONGLONG                    i; //counter
    LONGLONG                    disk_pointer;
    FILE_POSITION_INFORMATION   disk_cur_pos;
    LARGE_INTEGER               disk_position_special;
    LONGLONG                    progress;
    PZG_HANDLE_LIST             lockedhandles;

    //can't continue if diskname is NULL
    if (DiskName == NULL) return STATUS_INVALID_PARAMETER_1;

    //can't continue if erasing block size equals zero;
    if (EraseBlockSize == 0) return STATUS_INVALID_PARAMETER_2;

    //can't continue if can't lock volumes on drive
    lockedhandles = NULL;
    if (!NT_SUCCESS(status = ZgHEPELockAndDismountVolumes(DiskName, &lockedhandles)))
        return status;

    //trying to get disk handle
    RtlInitUnicodeString(&disk_name, DiskName);

    InitializeObjectAttributes(&disk_oa, &disk_name, OBJ_CASE_INSENSITIVE, NULL, NULL);
    
    status = NtOpenFile(
        &disk,
        FILE_READ_DATA | FILE_WRITE_DATA | SYNCHRONIZE,
        &disk_oa,
        &disk_isb,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
        FILE_SYNCHRONOUS_IO_NONALERT | FILE_NO_INTERMEDIATE_BUFFERING | FILE_WRITE_THROUGH | FILE_NON_DIRECTORY_FILE
     );

    if (!NT_SUCCESS(status))
    {
        DbgPrint("\r\nAUTOHEPE (ZgHEPEEraseHarddisk): can't acquire disk handle with status code 0x%08x\r\n", status);
        memset(disk_er1_msg_buf, 0, sizeof(WCHAR) * HEPE_OUTPUT_BUFFER_SIZE);
        _snwprintf(disk_er1_msg_buf, HEPE_OUTPUT_BUFFER_SIZE, HEPE_ERASING_ERA_MSG, status);
        RtlInitUnicodeString(&disk_er1_msg, disk_er1_msg_buf);
        NtDisplayString(&disk_er1_msg);
        return status;
    }

    //validating EraseBlockSize argument
    memset(&disk_geo, 0, sizeof(DISK_GEOMETRY));

    status = NtDeviceIoControlFile(
        disk,
        NULL,
        NULL,
        NULL,
        &disk_isb,
        IOCTL_DISK_GET_DRIVE_GEOMETRY,
        NULL,
        0,
        &disk_geo,
        sizeof(DISK_GEOMETRY)
    );

    if (!NT_SUCCESS(status))
    {
        DbgPrint("\r\nAUTOHEPE (ZgHEPEEraseHarddisk): can't acquire disk geometry with status code 0x%08x\r\n", status);
        memset(disk_er1_msg_buf, 0, sizeof(WCHAR) * HEPE_OUTPUT_BUFFER_SIZE);
        _snwprintf(disk_er1_msg_buf, HEPE_OUTPUT_BUFFER_SIZE, HEPE_ERASING_ERB_MSG, status);
        RtlInitUnicodeString(&disk_er1_msg, disk_er1_msg_buf);
        NtDisplayString(&disk_er1_msg);
        NtClose(disk);
        return status;
    }

    if (EraseBlockSize % disk_geo.BytesPerSector != 0)
    {
        DbgPrint("\r\nAUTOHEPE (ZgHEPEEraseHarddisk): erase block size must be multiple hard drive sector size!\r\n");
        memset(disk_er1_msg_buf, 0, sizeof(WCHAR) * HEPE_OUTPUT_BUFFER_SIZE);
        _snwprintf(disk_er1_msg_buf, HEPE_OUTPUT_BUFFER_SIZE, HEPE_ERASING_ERC_MSG, status);
        RtlInitUnicodeString(&disk_er1_msg, disk_er1_msg_buf);
        NtDisplayString(&disk_er1_msg);
        NtClose(disk);
        return STATUS_INVALID_PARAMETER_2;
    }

    //acquring disk size:
    memset(&disk_size, 0, sizeof(GET_LENGTH_INFORMATION));

    status = NtDeviceIoControlFile(
        disk,
        NULL,
        NULL,
        NULL,
        &disk_isb,
        IOCTL_DISK_GET_LENGTH_INFO,
        NULL,
        0,
        &disk_size,
        sizeof(GET_LENGTH_INFORMATION)
    );

    if (!NT_SUCCESS(status))
    {
        DbgPrint("\r\nAUTOHEPE (ZgHEPEEraseHarddisk): can't acquire disk size with status code 0x%08x\r\n", status);
        memset(disk_er1_msg_buf, 0, sizeof(WCHAR) * HEPE_OUTPUT_BUFFER_SIZE);
        _snwprintf(disk_er1_msg_buf, HEPE_OUTPUT_BUFFER_SIZE, HEPE_ERASING_ERD_MSG, status);
        RtlInitUnicodeString(&disk_er1_msg, disk_er1_msg_buf);
        NtDisplayString(&disk_er1_msg);
        NtClose(disk);
        return status;
    }

    //calculating and printing total blocks count
    disk_blocks_count = disk_size.Length.QuadPart / EraseBlockSize;
    memset(disk_tbc_msg_buf, 0, HEPE_OUTPUT_BUFFER_SIZE * sizeof(WCHAR));
    _snwprintf(disk_tbc_msg_buf, HEPE_OUTPUT_BUFFER_SIZE, 
        L"%ws %I64u\r\n\0", HEPE_ERASING_TBC_MSG, disk_blocks_count);
    RtlInitUnicodeString(&disk_tbc_msg, disk_tbc_msg_buf);
    NtDisplayString(&disk_tbc_msg);

    //preparing write buffer;
    disk_buf = ZgMalloc(EraseBlockSize);

    if (disk_buf == NULL)
    {
        DbgPrint("\r\nAUTOHEPE (ZgHEPEEraseHarddisk): insufficient resources to allocate erasing block!\r\n");
        memset(disk_er1_msg_buf, 0, sizeof(WCHAR) * HEPE_OUTPUT_BUFFER_SIZE);
        _snwprintf(disk_er1_msg_buf, HEPE_OUTPUT_BUFFER_SIZE, HEPE_ERASING_ERE_MSG, status);
        RtlInitUnicodeString(&disk_er1_msg, disk_er1_msg_buf);
        NtDisplayString(&disk_er1_msg);
        NtClose(disk);
        return STATUS_INSUFFICIENT_RESOURCES;
    }

    memset(disk_buf, 0, EraseBlockSize);

    //printing warning message
    RtlInitUnicodeString(&disk_wrn_msg, HEPE_ERASING_WRN_MSG);
    NtDisplayString(&disk_wrn_msg);

    //doing erasing harddisk in cycle;
    memset(&disk_position_special, 0, sizeof(LARGE_INTEGER));
    disk_position_special.HighPart = -1;
    disk_position_special.LowPart = FILE_USE_FILE_POINTER_POSITION;

    progress = 0;

    for (i = 0, disk_pointer = 0; (i < disk_blocks_count) 
        && (disk_pointer <= disk_size.Length.QuadPart); ++i, disk_pointer += EraseBlockSize)
    {
        //printing current progress
        memset(disk_cur_msg_buf, 0, HEPE_OUTPUT_BUFFER_SIZE * sizeof(WCHAR));
        progress = (disk_pointer * 100 / disk_size.Length.QuadPart) + 1; 
        _snwprintf(
            disk_cur_msg_buf, 
            HEPE_OUTPUT_BUFFER_SIZE, 
            L"\r%ws %I64u (%I64u%%)\0",
            HEPE_ERASING_CUR_MSG,
            i + 1,
            progress
        );
        RtlInitUnicodeString(&disk_cur_msg, disk_cur_msg_buf);
        NtDisplayString(&disk_cur_msg);

        //setting current position;
        memset(&disk_cur_pos, 0, sizeof(FILE_POSITION_INFORMATION));
        disk_cur_pos.CurrentByteOffset.QuadPart = disk_pointer;
        status = NtSetInformationFile(
            disk,
            &disk_isb,
            &disk_cur_pos,
            sizeof(FILE_POSITION_INFORMATION),
            FilePositionInformation
        );

        if (!NT_SUCCESS(status))
        {
            DbgPrint("\r\nAUTOHEPE (ZgHEPEEraseHarddisk): can't seek to disk position %I64u with status code 0x%08x\r\n", i + 1, status);
            memset(disk_er1_msg_buf, 0, sizeof(WCHAR) * HEPE_OUTPUT_BUFFER_SIZE);
            _snwprintf(disk_er1_msg_buf, HEPE_OUTPUT_BUFFER_SIZE, HEPE_ERASING_ER1_MSG, i + 1, status);
            RtlInitUnicodeString(&disk_er1_msg, disk_er1_msg_buf);
            NtDisplayString(&disk_er1_msg);
            continue;
        }

        //filling block by zeros
        status = NtWriteFile(
            disk,
            NULL,
            NULL,
            NULL,
            &disk_isb,
            disk_buf,
            EraseBlockSize,
            &disk_position_special,
            NULL
        );

        if (!NT_SUCCESS(status))
        {
            DbgPrint("\r\nAUTOHEPE (ZgHEPEEraseHarddisk): can't write to disk position %I64u with status code 0x%08x\r\n", i + 1, status);
            memset(disk_er2_msg_buf, 0, sizeof(WCHAR) * HEPE_OUTPUT_BUFFER_SIZE);
            _snwprintf(disk_er2_msg_buf, HEPE_OUTPUT_BUFFER_SIZE, HEPE_ERASING_ER2_MSG, i + 1, status);
            RtlInitUnicodeString(&disk_er2_msg, disk_er2_msg_buf);
            NtDisplayString(&disk_er2_msg);
            continue;
        }
    }

    //close'n free handle list
    ZgCloseAndFreeHandleList(&lockedhandles);
    
    //update drive information
    status = NtDeviceIoControlFile(
        disk,
        NULL,
        NULL,
        NULL,
        &disk_isb,
        IOCTL_DISK_UPDATE_PROPERTIES,
        NULL,
        0,
        NULL,
        0
    );

    if (!NT_SUCCESS(status))
       DbgPrint("\r\nAUTOHEPE (ZgHEPEEraseHarddisk): can't update drive geometry with status code 0x%08x\r\n", status);

    //performing clean up
    ZgFree(disk_buf);
    NtClose(disk);
    return STATUS_SUCCESS;
}

//*********************************************************
//
//   Function: NtProcessStartup
//
//   Accept arguments: args: PPEB
//
//   Return value: none
//
//   Info: main entry point of the application
//
//*********************************************************
void NtProcessStartup(PPEB args)
{
    UNICODE_STRING  welcome1;
    UNICODE_STRING  welcome2;
    UNICODE_STRING  welcome3;
    WCHAR           welcome3tmp[HEPE_OUTPUT_BUFFER_SIZE];
    UNICODE_STRING  welcome4;
    WCHAR           welcome4tmp[HEPE_OUTPUT_BUFFER_SIZE];
    ZG_HEPE_ARGS    hepeargs;
    UNICODE_STRING  opconfirmed;
    UNICODE_STRING  opcancelled;
    UNICODE_STRING  opcompleted;
    UNICODE_STRING  opnotcomplt;
    LARGE_INTEGER   delayaftercancel;
    UNICODE_STRING  osrestart;
    LARGE_INTEGER   delayaftersuccess;
    BOOLEAN         PrivLastState;
    NTSTATUS        status;

    //hepe can't work in safe mode
    if (GetKUserSharedData()->SafeBootMode == TRUE)
    {
        DbgPrint("\r\nAUTOHEPE (main): cannot work in safeboot mode! Exiting...\r\n");
        ZgRemoveHEPECF();
        ZgClearAutocheck();
        NtTerminateProcess(NtCurrentProcess(), STATUS_SUCCESS);
        return;
    }

    //preventing infinite loop
    if (!NT_SUCCESS(ZgValidateHEPE()))
    {
        DbgPrint("\r\nAUTOHEPE (main): HEPE was launched during last reboot! Exiting...\r\n");
        ZgClearAutocheck();
        NtTerminateProcess(NtCurrentProcess(), STATUS_SUCCESS);
        return;
    }

    //parsing args
    //we must create global process heap before starting;
    ZgInitHeap();

    if (!NT_SUCCESS(status = ZgParseHEPEArgs(args->ProcessParameters->CommandLine.Buffer, &hepeargs)))
    {
        DbgPrint("\r\nAUTOHEPE (main): HEPE faced the problem during argument parsing (status 0x%08x)\r\n", status);
        ZgFreeHeap();
        ZgRemoveHEPECF();
        ZgClearAutocheck();
        NtTerminateProcess(NtCurrentProcess(), STATUS_SUCCESS);
        return;
    }

    //displaying starting messages
    RtlInitUnicodeString(&welcome1, HEPE_HELLO_MSG1);
    NtDisplayString(&welcome1);
    RtlInitUnicodeString(&welcome2, HEPE_HELLO_MSG2);
    NtDisplayString(&welcome2);

    memset(welcome3tmp, 0, HEPE_OUTPUT_BUFFER_SIZE * sizeof(WCHAR));
    _snwprintf(welcome3tmp, HEPE_OUTPUT_BUFFER_SIZE, L"%ws %ws.\r\n\0", HEPE_HELLO_MSG3, hepeargs.drive);
    RtlInitUnicodeString(&welcome3, welcome3tmp);
    NtDisplayString(&welcome3);

    memset(welcome4tmp, 0, HEPE_OUTPUT_BUFFER_SIZE * sizeof(WCHAR));
    _snwprintf(welcome4tmp, HEPE_OUTPUT_BUFFER_SIZE, L"%ws %ld bytes.\r\n\0", HEPE_HELLO_MSG4, hepeargs.eraseblocksize);
    RtlInitUnicodeString(&welcome4, welcome4tmp);
    NtDisplayString(&welcome4);

    //providing delay before operation started;
    if (!NT_SUCCESS(status = ZgHEPEDelayExecution()))
    {
        if (status == STATUS_CANCELLED)
        {
            RtlInitUnicodeString(&opcancelled, HEPE_OPERATION_CANCELLED);
            NtDisplayString(&opcancelled);

            memset(&delayaftercancel, 0, sizeof(LARGE_INTEGER));
            delayaftercancel.QuadPart = HEPE_DELAY_AFTER_CANCEL;
            NtDelayExecution(FALSE, &delayaftercancel);
        }
        else
            DbgPrint("\r\nAUTOHEPE (main): HEPE faced the problem during delay (status 0x%08x)\r\n", status);

        ZgFreeHeap();
        ZgRemoveHEPECF();
        ZgClearAutocheck();
        NtTerminateProcess(NtCurrentProcess(), STATUS_SUCCESS);
        return;
    }

    //if delay was not interrupted we can start operation now
    RtlInitUnicodeString(&opconfirmed, HEPE_OPERATION_CONFIRMED);
    NtDisplayString(&opconfirmed);

    //doing erasing operation (finally!)
    if (!NT_SUCCESS(status = ZgHEPEEraseHarddisk(hepeargs.drive, hepeargs.eraseblocksize)))
    {
         DbgPrint("\r\nAUTOHEPE (main): HEPE can't complete erasing operation! (status 0x%08x)\r\n", status);
         RtlInitUnicodeString(&opnotcomplt, HEPE_OPERATION_NOTCOMPLT);
         NtDisplayString(&opnotcomplt);
         ZgFreeHeap();
         ZgRemoveHEPECF();
         ZgClearAutocheck();
         memset(&delayaftercancel, 0, sizeof(LARGE_INTEGER));
         delayaftercancel.QuadPart = HEPE_DELAY_AFTER_CANCEL;
         NtDelayExecution(FALSE, &delayaftercancel);
         NtTerminateProcess(NtCurrentProcess(), STATUS_SUCCESS);
         return;
    }
    else
    {
        RtlInitUnicodeString(&opcompleted, HEPE_OPERATION_COMPLETED);
        NtDisplayString(&opcompleted);
    }

    //after all we need that is reboot the machine
    RtlInitUnicodeString(&osrestart, HEPE_OSRESTART_MSG);
    NtDisplayString(&osrestart);

    memset(&delayaftersuccess, 0, sizeof(LARGE_INTEGER));
    delayaftersuccess.QuadPart = HEPE_DELAY_AFTER_SUCCESS;
    NtDelayExecution(FALSE, &delayaftersuccess);

    RtlAdjustPrivilege(SE_SHUTDOWN_PRIVILEGE, TRUE, FALSE, &PrivLastState);
    NtShutdownSystem(ShutdownReboot);
    NtTerminateProcess(NtCurrentProcess(), STATUS_SUCCESS);
}