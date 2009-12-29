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

#  include "options.h"

#ifdef USE_XML

/* #define XMLDEBUG 1 */

#include <string.h>
#include <libxml/xmlreader.h>

#include "runtime.h"
#include "table.h"
#include "xml.h"

static struct vector *zero_vector;

static xmlCharEncodingHandlerPtr xml_utf16_encoder;

static char *strdup_utf8(const xmlChar *xmlstr)
{
  unsigned char *result;
  int ilen, olen, i;

  if (xmlstr == NULL)
    return NULL;

  
  ilen = strlen((const char *)xmlstr);
  olen = ilen * 2;
  result = malloc(olen + 1);
  if (xml_utf16_encoder->output(result, &olen, xmlstr, &ilen) < 0)
    {
      free(result);
      return NULL;
    }

  for (i = 0; i < olen / 2; ++i)
    {
      if (result[i * 2 + 1] == 0)
        result[i] = result[i * 2];
      else
        result[i] = '?';
    }
  result[i] = 0;

  return (char *)result;
}

static struct string *alloc_utf8(const xmlChar *xmlstr)
{
  struct string *mresult;
  char *latin1;

  if (xmlstr == NULL)
    return NULL;

  latin1 = strdup_utf8(xmlstr);
  if (latin1 == NULL)
    return NULL;

  mresult = alloc_string(latin1);
  free(latin1);

  return mresult;
}

static struct {
  char *msg;
  char *uri;
  int line;
} xml_error;

static void xml_error_handler(void *arg, 
                              const char *msg, 
                              xmlParserSeverities severity, 
                              xmlTextReaderLocatorPtr locator)
{
  xml_error.uri  = strdup_utf8(xmlTextReaderLocatorBaseURI(locator));
  xml_error.line = xmlTextReaderLocatorLineNumber(locator);
  /* msg is hopefully ASCII */
  xml_error.msg  = strdup(msg);
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

SECTOP(xml_read_file, 0, "`s `n `t -> `x. Reads an XML document from file `s,"
      " using `XML_PARSE_xxx flags in `n. Returns an XML tree, or"
       " an error string. `t is the name table, or NULL.",
      3, (struct string *mfilename, value mflags,
           struct table *name_table), LVL_IMPLEMENTOR, 0,
       "snx.x")
{
  struct vector *xmlstack = NULL, *node = NULL;
  struct gcpro gcpro1, gcpro2, gcpro3;
  const char *error = NULL;
  char *filename;
  int flags;
  int result;

  xmlTextReaderPtr reader;

  TYPEIS(mfilename, type_string);
  flags = GETINT(mflags);
  
  LOCALSTR(filename, mfilename);

  reader = xmlReaderForFile(filename, NULL, flags);

  if (reader == NULL)
    return msprintf("cannot open '%s'", filename);

  xmlTextReaderSetErrorHandler(reader, xml_error_handler, NULL);

  if (name_table == NULL)
    name_table = alloc_table(DEF_TABLE_SIZE);
  else
    TYPEIS(name_table, type_table);

  GCPRO3(xmlstack, node, name_table);
  while ((result = xmlTextReaderRead(reader)) == 1)
    {
      value mdepth = makeint(xmlTextReaderDepth(reader));
      int node_type = xmlTextReaderNodeType(reader);
      int nattrs, i;
      char *name;
      struct symbol *name_symbol;

      node = alloc_vector(xmlnode_entries);

      node->data[xmlnode_depth] = mdepth;
      node->data[xmlnode_type] = makeint(node_type);

      name = strdup_utf8(xmlTextReaderConstName(reader));

      if (!table_lookup(name_table, name, &name_symbol))
        {
          struct string *mstr;

          if (((struct obj *)name_table)->flags & OBJ_READONLY)
            {
              free(name);
              xmlFreeTextReader(reader);
              runtime_error(error_value_read_only);
            }

          mstr = alloc_string(name);
          name_symbol = table_add_fast(name_table,
                                       mstr,
                                       NULL);
        }
      free(name);
      SET_VECTOR(node, xmlnode_name, name_symbol);

      SET_VECTOR(node, xmlnode_value,
                 alloc_utf8(xmlTextReaderConstValue(reader)));

      nattrs = xmlTextReaderAttributeCount(reader);
      SET_VECTOR(node, xmlnode_attributes,
                 nattrs ? alloc_vector(nattrs) : zero_vector);
      for (i = 0; i < nattrs; ++i)
        {
          struct list *pair = alloc_list(NULL, NULL);
          struct gcpro gcpro4;
          struct string *mstr;

          GCPRO(gcpro4, pair);

          xmlTextReaderMoveToAttributeNo(reader, i);

          mstr = alloc_utf8(xmlTextReaderConstName(reader));
          pair->car = mstr;

          mstr = alloc_utf8(xmlTextReaderConstValue(reader));
          pair->cdr = mstr;

          UNGCPRO1(gcpro4);

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
          struct vector *parent;

          while (mdepth < xmlstack->data[xmlnode_depth])
            {
              xmlstack = xmlstack->data[xmlnode_parent];
              xmlstack->data[xmlnode_children] =
                reverse_siblings(xmlstack->data[xmlnode_children]);
            }

          if (node_type != XML_READER_TYPE_END_ELEMENT
              && node_type != XML_READER_TYPE_END_ENTITY)
            {
              parent = xmlstack->data[xmlnode_parent];
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
  if (result == -1)
    error = "does not parse";

  xmlFreeTextReader(reader);

  if (xml_error.msg)
    {
      struct string *merror = msprintf("%s:%d: %s",
                                       xml_error.uri,
                                       xml_error.line,
                                       xml_error.msg);
      free(xml_error.msg);
      free(xml_error.uri);
      memset(&xml_error, 0, sizeof xml_error);
      return merror;
    }

  return error ? (value)alloc_string(error) : reverse_siblings(xmlstack);
}

#define DEFINE_INT(name) system_define(#name, makeint(name))

void xml_init(void)
{
  xml_utf16_encoder = xmlFindCharEncodingHandler("UTF-16LE");
  assert(xml_utf16_encoder != NULL);

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

  zero_vector = alloc_vector(0);
  staticpro(&zero_vector);
}

#else  /* ! USE_XML */

void xml_init(void)
{
}

#endif /* ! USE_XML */
