#define CAML_NAME_SPACE
// OCaml declarations
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/fail.h>

#import <Foundation/Foundation.h>

CAMLprim value json_string_of_plist(value plist_raw)
{
  CAMLparam1(plist_raw);
  CAMLlocal1(as_json_string);

  NSString *source =
    [NSString stringWithUTF8String:caml_strdup(String_val(plist_raw))];
  NSData *plist_data = [source dataUsingEncoding:NSUTF8StringEncoding];
  NSPropertyListFormat format;
  NSDictionary *plist =
    [NSPropertyListSerialization propertyListWithData:plist_data
					      options:NSPropertyListImmutable
					       format:&format
						error:NULL];
  NSError *error;
  NSData *json_data =
    [NSJSONSerialization dataWithJSONObject:plist
				    options:NSJSONWritingPrettyPrinted
				      error:&error];
  NSString *json_string =
    [[NSString alloc] initWithData:json_data
			  encoding:NSUTF8StringEncoding];
  CAMLreturn(caml_copy_string([json_string UTF8String]));
}
