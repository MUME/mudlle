/*
 * Copyright (c) 1993-2012 David Gay and Gustav Hållberg
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

#include "../mudlle-config.h"

#ifdef USE_XML

/* #define XMLDEBUG 1 */

#include <string.h>

#include <libxml/xmlreader.h>

#include "../ports.h"
#include "../table.h"

#include "check-types.h"
#include "mudlle-xml.h"
#include "prims.h"
#include "symbol.h"
#include "vector.h"

static xmlCharEncodingHandlerPtr xml_utf16_encoder;

static char *strdup_utf8(const xmlChar *xmlstr)
{
  if (xmlstr == NULL)
    return NULL;

  /* optimize for ASCII input */
  for (const xmlChar *c = xmlstr; ; ++c)
    if (*c & 0x80)
      break;
    else if (*c == 0)
      {
        size_t size = c - xmlstr + 1;
        char *result = malloc(size);
        memcpy(result, xmlstr, size);
        return result;
      }

  int ilen = strlen((const char *)xmlstr);
  int olen = ilen * 2;
  unsigned char *result = malloc(olen + 1);
  if (xml_utf16_encoder->output(result, &olen, xmlstr, &ilen) < 0)
    {
      free(result);
      return NULL;
    }

  int ochars = olen / 2;
  for (int i = 0; i < ochars; ++i)
    {
      if (result[i * 2 + 1] == 0)
        result[i] = result[i * 2];
      else
        result[i] = '?';
    }
  result[ochars] = 0;
  return realloc(result, ochars + 1);
}

static struct string *alloc_utf8(const xmlChar *xmlstr)
{
  if (xmlstr == NULL)
    return NULL;

  /* optimize for ASCII input */
  for (const xmlChar *c = xmlstr; ; ++c)
    if (*c & 0x80)
      break;
    else if (*c == 0)
      return alloc_string((const char *)xmlstr);

  char *latin1 = strdup_utf8(xmlstr);
  if (latin1 == NULL)
    return NULL;

  struct string *mresult = alloc_string(latin1);
  free(latin1);

  return mresult;
}

struct xml_error {
  char *msg;
  char *uri;
  int line;
};

static void xml_error_handler(void *arg,
                              const char *msg,
                              xmlParserSeverities severity,
                              xmlTextReaderLocatorPtr locator)
{
  struct xml_error *xml_error = arg;
  if (xml_error->msg != NULL)
    return;
  xmlChar *uri = xmlTextReaderLocatorBaseURI(locator);
  *xml_error = (struct xml_error){
    .uri  = strdup_utf8(uri),
    .line = xmlTextReaderLocatorLineNumber(locator),
    /* msg is hopefully ASCII */
    .msg  = strdup(msg)
  };
  free(uri);
}

static struct vector *reverse_siblings(struct vector *node)
{
  struct vector *prev = NULL;

  while (node)
    {
      struct vector *next = node->data[xmlnode_sibling];
      node->data[xmlnode_sibling] = prev;
      prev = node;
      node = next;
    }

  return prev;
}

static value mudlle_xml_read(xmlTextReaderPtr reader, struct table *name_table)
{
  struct xml_error xml_error = { .msg = NULL };
  xmlTextReaderSetErrorHandler(reader, xml_error_handler, &xml_error);

  if (name_table == NULL)
    name_table = alloc_table(DEF_TABLE_SIZE);
  else
    TYPEIS(name_table, table);

  struct vector *node = NULL, *xmlstack = NULL;
  GCPRO(xmlstack, node, name_table);
  int result;
  while ((result = xmlTextReaderRead(reader)) == 1)
    {
      value mdepth = makeint(xmlTextReaderDepth(reader));
      int node_type = xmlTextReaderNodeType(reader);

      node = alloc_vector(xmlnode_entries);

      node->data[xmlnode_depth] = mdepth;
      node->data[xmlnode_type] = makeint(node_type);

      char *name = strdup_utf8(xmlTextReaderConstName(reader));

      struct symbol *name_symbol = table_lookup(name_table, name);
      if (name_symbol == NULL)
        {
          struct string *mstr = make_readonly(alloc_string(name));

          if (obj_readonlyp(&name_table->o))
            {
              free(name);
              xmlFreeTextReader(reader);

              /* cause user-friendly call trace */
              code_table_set(name_table, mstr, NULL);
              abort();
            }

          name_symbol = table_add_fast(name_table, mstr, NULL);
        }
      free(name);
      SET_VECTOR(node, xmlnode_name, name_symbol);

      SET_VECTOR(node, xmlnode_value,
                 alloc_utf8(xmlTextReaderConstValue(reader)));

      int nattrs = xmlTextReaderAttributeCount(reader);
      SET_VECTOR(node, xmlnode_attributes,
                 nattrs ? alloc_vector(nattrs) : empty_vector);
      for (int i = 0; i < nattrs; ++i)
        {
          struct list *pair = alloc_list(NULL, NULL);
          struct gcpro gcpro4;
          struct string *mstr;

          GCPROV(gcpro4, pair);

          xmlTextReaderMoveToAttributeNo(reader, i);

          mstr = alloc_utf8(xmlTextReaderConstName(reader));
          pair->car = mstr;

          mstr = alloc_utf8(xmlTextReaderConstValue(reader));
          pair->cdr = mstr;

          UNGCPROV(gcpro4);

          ((struct vector *)node->data[xmlnode_attributes])->data[i] = pair;
        }

#ifdef XMLDEBUG
      fprintf(stderr, "%*s  depth %3d  type %3d  name %s\n",
              (int)intval(mdepth), "",
              (int)intval(mdepth),
              (int)xmlTextReaderNodeType(reader),
              xmlTextReaderConstName(reader));
#endif

      if (xmlstack == NULL)
        xmlstack = node;
      else if (mdepth > xmlstack->data[xmlnode_depth])
        {
          node->data[xmlnode_parent] = xmlstack;
          node->data[xmlnode_sibling] = xmlstack->data[xmlnode_children];
          xmlstack->data[xmlnode_children] = node;
          xmlstack = node;
        }
      else
        {
          while (mdepth < xmlstack->data[xmlnode_depth])
            {
              xmlstack = xmlstack->data[xmlnode_parent];
              xmlstack->data[xmlnode_children] =
                reverse_siblings(xmlstack->data[xmlnode_children]);
            }

          if (node_type != XML_READER_TYPE_END_ELEMENT
              && node_type != XML_READER_TYPE_END_ENTITY)
            {
              struct vector *parent = xmlstack->data[xmlnode_parent];
              node->data[xmlnode_parent] = parent;
              node->data[xmlnode_sibling] = xmlstack;
              if (parent)
                parent->data[xmlnode_children] = node;
              xmlstack = node;
            }
        }
    }
  UNGCPRO();

  /* will probably not happen without the "real" error strings being
     set; better safe than sorry! */
  if (result == -1 && xml_error.msg == NULL)
    {
      xmlChar *uri = xmlTextReaderBaseUri(reader);
      xml_error = (struct xml_error){
        .uri  = strdup_utf8(uri),
        .line = 1,
        .msg  = strdup("does not parse")
      };
      free(uri);
    }

  xmlFreeTextReader(reader);

  if (xml_error.msg)
    {
      struct vector *e = alloc_vector(3);
      struct gcpro gcpro4;
      GCPROV(gcpro4, e);
      SET_VECTOR(e, 0, alloc_string(xml_error.uri));
      SET_VECTOR(e, 1, makeint(xml_error.line));
      SET_VECTOR(e, 2, alloc_string(xml_error.msg));
      UNGCPROV(gcpro4);
      free(xml_error.uri);
      free(xml_error.msg);
      return e;
    }

  return reverse_siblings(xmlstack);
}

struct xml_read_data {
  struct vector *minput;
  int vidx, sidx;
};

static int xml_read_input(void *arg, char *buffer, int len)
{
  struct xml_read_data *read_data = arg;
  int count = 0;
  while (len > 0)
    {
      if (vector_len(read_data->minput) == read_data->vidx)
        return count;

      struct string *s = read_data->minput->data[read_data->vidx];
      int srem = string_len(s) - read_data->sidx;
      if (srem == 0)
        {
          ++read_data->vidx;
          read_data->sidx = 0;
          continue;
        }
      int n = srem;
      if (len < n)
        n = len;
      memcpy(buffer, s->str + read_data->sidx, n);
      buffer += n;
      count += n;
      len -= n;
      read_data->sidx += n;
    }
  return count;
}

static int xml_read_close(void *arg)
{
  return 0;
}

SECOP(xml_read, , "`v `s `n `t -> `x. Reads an XML document (named `s)"
       " from vector of strings `v using `XML_PARSE_xxx flags in `n.\n"
       "Returns an XML tree, or an error [`uri `line `msg].\n"
       "`t is the name table, or NULL.",
       (struct vector *minput, struct string *murl, value mflags,
           struct table *name_table),
       LVL_IMPLEMENTOR, 0,
       "vsn[tu].v")
{
  int flags;
  CHECK_TYPES(minput,     vector,
              murl,       string,
              mflags,     CT_AUTO_RANGE(flags),
              name_table, CT_TYPES(null, table));

  char *url;
  ALLOCA_PATH(url, murl);

  for (int i = 0; i < vector_len(minput); ++i)
    TYPEIS(minput->data[i], string);

  struct xml_read_data read_data = { .minput = minput };
  GCPRO(read_data.minput);
  xmlTextReaderPtr reader = xmlReaderForIO(xml_read_input, xml_read_close,
                                           &read_data, url, "ISO-8859-1",
                                           flags);
  if (reader == NULL)
    runtime_error(error_bad_value);

  value result = mudlle_xml_read(reader, name_table);

  UNGCPRO();

  return result;
}

SECOP(xml_read_file, , "`s `n `t -> `x. Reads an XML document from file `s,"
      " using `XML_PARSE_xxx flags in `n. Returns an XML tree, or"
      " an error [`uri `line `msg]. `t is the name table, or NULL.",
      (struct string *mfilename, value mflags, struct table *name_table),
      LVL_IMPLEMENTOR, 0,
      "sn[tu].v")
{
  int flags;
  CHECK_TYPES(mfilename,  string,
              mflags,     CT_AUTO_RANGE(flags),
              name_table, CT_TYPES(null, table));

  char *filename;
  ALLOCA_PATH(filename, mfilename);

  xmlTextReaderPtr reader = xmlReaderForFile(filename, NULL, flags);
  if (reader == NULL)
    {
      GCPRO(mfilename);
      struct vector *e = alloc_vector(3);
      UNGCPRO();
      e->data[0] = mfilename;
      e->data[1] = makeint(0);
      STATIC_STRING(sstr_cannot_open, "cannot open file");
      e->data[2] = GET_STATIC_STRING(sstr_cannot_open);
      return e;
    }

  return mudlle_xml_read(reader, name_table);
}

#define DEFINE_INT(name) system_define(#name, makeint(name))

void xml_init(void)
{
  xml_utf16_encoder = xmlFindCharEncodingHandler("UTF-16LE");
  assert(xml_utf16_encoder != NULL);

  DEFINE(xml_read);
  DEFINE(xml_read_file);

  DEFINE_INT(XML_PARSE_RECOVER);
  DEFINE_INT(XML_PARSE_NOENT);
  DEFINE_INT(XML_PARSE_DTDLOAD);
  DEFINE_INT(XML_PARSE_DTDATTR);
  DEFINE_INT(XML_PARSE_DTDVALID);
  DEFINE_INT(XML_PARSE_NOERROR);
  DEFINE_INT(XML_PARSE_NOWARNING);
  DEFINE_INT(XML_PARSE_PEDANTIC);
  DEFINE_INT(XML_PARSE_NOBLANKS);
  DEFINE_INT(XML_PARSE_SAX1);
  DEFINE_INT(XML_PARSE_XINCLUDE);
  DEFINE_INT(XML_PARSE_NONET);
  DEFINE_INT(XML_PARSE_NODICT);
  DEFINE_INT(XML_PARSE_NSCLEAN);
  DEFINE_INT(XML_PARSE_NOCDATA);
  DEFINE_INT(XML_PARSE_NOXINCNODE);
#if LIBXML_VERSION >= 20626
  DEFINE_INT(XML_PARSE_COMPACT);
#endif

  DEFINE_INT(XML_READER_TYPE_NONE);
  DEFINE_INT(XML_READER_TYPE_ELEMENT);
  DEFINE_INT(XML_READER_TYPE_ATTRIBUTE);
  DEFINE_INT(XML_READER_TYPE_TEXT);
  DEFINE_INT(XML_READER_TYPE_CDATA);
  DEFINE_INT(XML_READER_TYPE_ENTITY_REFERENCE);
  DEFINE_INT(XML_READER_TYPE_ENTITY);
  DEFINE_INT(XML_READER_TYPE_PROCESSING_INSTRUCTION);
  DEFINE_INT(XML_READER_TYPE_COMMENT);
  DEFINE_INT(XML_READER_TYPE_DOCUMENT);
  DEFINE_INT(XML_READER_TYPE_DOCUMENT_TYPE);
  DEFINE_INT(XML_READER_TYPE_DOCUMENT_FRAGMENT);
  DEFINE_INT(XML_READER_TYPE_NOTATION);
  DEFINE_INT(XML_READER_TYPE_WHITESPACE);
  DEFINE_INT(XML_READER_TYPE_SIGNIFICANT_WHITESPACE);
  DEFINE_INT(XML_READER_TYPE_END_ELEMENT);
  DEFINE_INT(XML_READER_TYPE_END_ENTITY);
  DEFINE_INT(XML_READER_TYPE_XML_DECLARATION);
}

#else  /* ! USE_XML */

void xml_init(void)
{
}

#endif /* ! USE_XML */
