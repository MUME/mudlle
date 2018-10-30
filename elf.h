#ifndef ELF_H
#define ELF_H

#include <stddef.h>
#include <stdint.h>

/* See https://www.uclibc.org/docs/elf-64-gen.pdf */

typedef uintptr_t elf_addr_t;

struct elf_file_hdr
{
  struct {
    uint8_t mag[4];
    uint8_t cls, data, version, osabi, abiversion;
    uint8_t pad[7];
  } e_ident;
  uint16_t e_type;              /* Object file type */
  uint16_t e_machine;           /* Machine type */
  uint32_t e_version;           /* Object file version */
  elf_addr_t e_entry;             /* Entry point address */
  elf_addr_t e_phoff;             /* Program header offset */
  elf_addr_t e_shoff;             /* Section header offset */
  uint32_t e_flags;             /* Processor-specific flags */
  uint16_t e_ehsize;            /* ELF header size */
  uint16_t e_phentsize;         /* Size of program header entry */
  uint16_t e_phnum;             /* Number of program header entries */
  uint16_t e_shentsize;         /* Size of section header entry */
  uint16_t e_shnum;             /* Number of section header entries */
  uint16_t e_shstrndx;          /* Section name string table index */
};

struct elf_sect_hdr
{
  uint32_t sh_name;             /* Section name */
  uint32_t sh_type;             /* Section type */
  elf_addr_t sh_flags;            /* Section attributes */
  elf_addr_t sh_addr;             /* Virtual address in memory */
  elf_addr_t sh_offset;           /* Offset in file */
  elf_addr_t sh_size;             /* Size of section */
  uint32_t sh_link;             /* Link to other section */
  uint32_t sh_info;             /* Miscellaneous information */
  elf_addr_t sh_addralign;        /* Address alignment boundary */
  elf_addr_t sh_entsize;          /* Size of entries, if section has table */
};

struct elf_sym_entry
{
  uint32_t st_name;             /* Symbol name */
#ifdef __x86_64__
  uint8_t st_info;              /* Type and Binding attributes */
  uint8_t st_other;             /* Reserved */
  uint16_t st_shndx;            /* Section table index */
  uint64_t st_value;            /* Symbol value */
  uint64_t st_size;             /* Size of object (e.g., common) */
#elif defined __i386__
  elf_addr_t st_value;          /* Symbol value */
  elf_addr_t st_size;           /* Size of object (e.g., common) */
  uint8_t st_info;              /* Type and Binding attributes */
  uint8_t st_other;             /* Reserved */
  uint16_t st_shndx;            /* Section table index */
#endif
};

struct elf_file {
  struct elf_sect_data {
    struct elf_sect_hdr hdr;
    const void *data;
    unsigned sect;
  } **sections;
  struct elf_file_hdr file_hdr;
};

enum {
  ELFCLASS32 = 1,
  ELFCLASS64 = 2,
#ifdef __x86_64__
  ELFCLASS = ELFCLASS64,
#elif defined __i386__
  ELFCLASS = ELFCLASS32,
#endif

  ELFDATA2LSB   = 1,
  ELFOSABI_SYSV = 0,

  EV_CURRENT    = 1,

  STB_LOCAL = 0,

  STT_FUNC  = 2,
  STT_FILE  = 4,

  SHN_ABS = 0xfff1,

  EM_386    = 3,
  EM_X86_64 = 62,

  ET_REL = 1,

  SHF_ALLOC     = 1 << 1,
  SHF_EXECINSTR = 1 << 2,
  SHF_STRINGS   = 1 << 5,
};

enum elf_sect_type {
  SHT_NULL     = 0,
  SHT_PROGBITS = 1,
  SHT_SYMTAB   = 2,
  SHT_STRTAB   = 3,
  SHT_NOBITS   = 8
};

struct elf_sect_data *elf_alloc_sect(struct elf_file *elf,
                                     unsigned name,
                                     enum elf_sect_type type,
                                     unsigned flags,
                                     unsigned align);
void elf_init(struct elf_file *elf);
void elf_set_sect_data(struct elf_sect_data *sect,
                       const void *data, size_t size);
void *elf_alloc(struct elf_file *elf, size_t *size);
void elf_free(struct elf_file *elf);

#endif  /* ELF_H */
