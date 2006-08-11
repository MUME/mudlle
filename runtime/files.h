/*
 * Copyright (c) 1993-2006 David Gay and Gustav Hållberg
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose, without fee, and without written agreement is hereby granted,
 * provided that the above copyright notice and the following two paragraphs
 * appear in all copies of this software.
 * 
 * IN NO EVENT SHALL DAVID GAY OR GUSTAV HALLBERG BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF DAVID GAY OR
 * GUSTAV HALLBERG HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * DAVID GAY AND GUSTAV HALLBERG SPECIFICALLY DISCLAIM ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN
 * "AS IS" BASIS, AND DAVID GAY AND GUSTAV HALLBERG HAVE NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#ifndef FILES_H
#define FILES_H

enum {
  FS_DEV     =  0,
  FS_INO     =  1,
  FS_MODE    =  2,
  FS_NLINK   =  3,
  FS_UID     =  4,
  FS_GID     =  5,
  FS_RDEV    =  6,
  FS_SIZE    =  7,
  FS_ATIME   =  8,
  FS_MTIME   =  9,
  FS_CTIME   = 10,
  FS_BLKSIZE = 11,
  FS_BLOCKS  = 12,
  FILE_STAT_FIELDS
};

enum {
  PW_NAME    = 0,
  PW_UID     = 1,
  PW_GID     = 2,
  PW_GECOS   = 3,
  PW_DIR     = 4,
  PW_SHELL   = 5,
  PASSWD_ENTRY_FIELDS
};

enum {
  GR_NAME    = 0,
  GR_GID     = 1,
  GR_MEM     = 2,
  GROUP_ENTRY_FIELDS
};

void files_init(void);

#endif
