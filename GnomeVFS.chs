-- This is Haskell
#include "libgnomevfs/gnome-vfs.h"

module GnomeVFS where

import C2HS;

-- gboolean gnome_vfs_init();
init :: IO Bool
init = {#fun gnome_vfs_init {} -> `Bool' #}

-- void gnome_vfs_shutdown();
shutdown :: IO ()
shutdown = {#fun gnome_vfs_shutdown {} -> `()' #}

peekCStringOrNull :: CString -> IO (Maybe String)
peekCStringOrNull cs = if cs == nullPtr then return Nothing else do
	s <- peekCString cs
	return (Just s)

-- char* gnome_vfs_get_mime_type (const char* text_uri);
getMIMEType :: String -> IO (Maybe String)
getMIMEType = {#fun gnome_vfs_get_mime_type {`String'} -> `Maybe String' peekCStringOrNull* #}

-- char* gnome_vfs_uri_make_full_from_relative(const char *base_uri,const char *relative_uri);
uriMakeFullFromRelative :: String -> String -> IO String;
uriMakeFullFromRelative  = {#fun gnome_vfs_uri_make_full_from_relative {`String',`String'} -> `String' #}

