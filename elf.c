#include <stdlib.h>
#include <string.h>

#include "elf.h"

struct elf_sect_data *elf_alloc_sect(struct elf_file *elf,
                                     unsigned name,
                                     enum elf_sect_type type,
                                     unsigned flags,
                                     unsigned align)
{
  unsigned section = elf->file_hdr.e_shnum++;
  elf->sections = realloc(elf->sections,
                          sizeof elf->sections[0] * elf->file_hdr.e_shnum);
  struct elf_sect_data *data = malloc(sizeof *data);
  *data = (struct elf_sect_data){
    .hdr = {
      .sh_name      = name,
      .sh_type      = type,
      .sh_flags     = flags,
      .sh_addralign = align
    },
    .sect = section,
  };
  elf->sections[section] = data;
  return data;
}

void elf_init(struct elf_file *elf)
{
  *elf = (struct elf_file){
    .file_hdr = {
      .e_ident = {
        .mag = "\x7f" "ELF",
        .cls        = ELFCLASS,
        .data       = ELFDATA2LSB,
        .version    = EV_CURRENT,
        .osabi      = ELFOSABI_SYSV,
        .abiversion = 0,
      },
      .e_type    = ET_REL,
      .e_version = EV_CURRENT,
#ifdef __x86_64__
      .e_machine = EM_X86_64,
#elif defined __i386__
      .e_machine = EM_386,
#else
#error Unsupported architecture
#endif
    }
  };
  elf_alloc_sect(elf, 0, 0, 0, 0);
}

void elf_set_sect_data(struct elf_sect_data *sect,
                       const void *data, size_t size)
{
  sect->data = data;
  sect->hdr.sh_size = size;
}

void *elf_alloc(struct elf_file *elf, size_t *size)
{
  size_t tsize = sizeof elf->file_hdr;
  for (unsigned n = 0; n < elf->file_hdr.e_shnum; ++n)
    {
      struct elf_sect_data *data = elf->sections[n];
      if (data->hdr.sh_type != SHT_NULL)
        {
          unsigned align = data->hdr.sh_addralign;
          tsize = (tsize + align - 1) & -align;
          data->hdr.sh_offset = tsize;
          if (data->hdr.sh_type != SHT_NOBITS)
            tsize += data->hdr.sh_size;
        }
    }
  elf->file_hdr.e_shoff = tsize;
  elf->file_hdr.e_ehsize = sizeof elf->file_hdr;
  elf->file_hdr.e_shentsize = sizeof elf->sections[0]->hdr;
  tsize += elf->file_hdr.e_shnum * elf->file_hdr.e_shentsize;

  *size = tsize;

  char *result = malloc(tsize);
  *(struct elf_file_hdr *)result = elf->file_hdr;
  for (unsigned n = 0; n < elf->file_hdr.e_shnum; ++n)
    {
      struct elf_sect_data *data = elf->sections[n];
      if (data->hdr.sh_type != SHT_NOBITS && data->hdr.sh_type != SHT_NULL)
        memcpy(result + data->hdr.sh_offset,
               data->data,
               data->hdr.sh_size);
      ((struct elf_sect_hdr *)(result + elf->file_hdr.e_shoff))[n] = data->hdr;
    }
  return result;
}

void elf_free(struct elf_file *elf)
{
  for (unsigned n = 0; n < elf->file_hdr.e_shnum; ++n)
    free(elf->sections[n]);
  free(elf->sections);
  *elf = (struct elf_file){ 0 };
}

