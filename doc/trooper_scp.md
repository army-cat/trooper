

# Module trooper_scp #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Trooper SCP is in charge of handle remote/local files copy.

<a name="description"></a>

## Description ##
You need to have a tropper SSH connection opened to list
remote files, upload local files to the remote place or
download remote files.
<a name="types"></a>

## Data Types ##




### <a name="type-file_handler">file_handler()</a> ###


__abstract datatype__: `file_handler()`




### <a name="type-reason">reason()</a> ###


<pre><code>
reason() = atom()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td>Closes the handler.</td></tr><tr><td valign="top"><a href="#del_dir-2">del_dir/2</a></td><td>Removes a remote directory.</td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td>Deletes a remote file.</td></tr><tr><td valign="top"><a href="#list_dir-2">list_dir/2</a></td><td>List remote directory.</td></tr><tr><td valign="top"><a href="#make_dir-2">make_dir/2</a></td><td>Creates a remote directory.</td></tr><tr><td valign="top"><a href="#make_symlink-3">make_symlink/3</a></td><td>Makes a symlink in the a remote server.</td></tr><tr><td valign="top"><a href="#open-3">open/3</a></td><td>Opens a remote file using a handler to let use read and write.</td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td>Reads information from an opened remote file.</td></tr><tr><td valign="top"><a href="#read_file-2">read_file/2</a></td><td>Reads a remote file content.</td></tr><tr><td valign="top"><a href="#rename-3">rename/3</a></td><td>Renames a remote file.</td></tr><tr><td valign="top"><a href="#write-2">write/2</a></td><td>Writes information to an opened remote file.</td></tr><tr><td valign="top"><a href="#write_file-3">write_file/3</a></td><td>Writes a remote file content.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(File_handler::<a href="#type-file_handler">file_handler()</a>) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

Closes the handler.

<a name="del_dir-2"></a>

### del_dir/2 ###

<pre><code>
del_dir(Trooper::<a href="trooper_ssh.md#type-trooper">trooper_ssh:trooper()</a>, Name::string()) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

Removes a remote directory.

<a name="delete-2"></a>

### delete/2 ###

<pre><code>
delete(Trooper::<a href="trooper_ssh.md#type-trooper">trooper_ssh:trooper()</a>, Name::string()) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

Deletes a remote file.

<a name="list_dir-2"></a>

### list_dir/2 ###

<pre><code>
list_dir(Trooper::<a href="trooper_ssh.md#type-trooper">trooper_ssh:trooper()</a>, Path::string()) -&gt; {ok, [string()]} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

List remote directory.

<a name="make_dir-2"></a>

### make_dir/2 ###

<pre><code>
make_dir(Trooper::<a href="trooper_ssh.md#type-trooper">trooper_ssh:trooper()</a>, Name::string()) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

Creates a remote directory.

<a name="make_symlink-3"></a>

### make_symlink/3 ###

<pre><code>
make_symlink(Trooper::<a href="trooper_ssh.md#type-trooper">trooper_ssh:trooper()</a>, Name::string(), Target::string()) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

Makes a symlink in the a remote server.

<a name="open-3"></a>

### open/3 ###

<pre><code>
open(File_handler::<a href="#type-file_handler">file_handler()</a>, Name::string(), Mode::<a href="ssh_sftp.md#type-mode">ssh_sftp:mode()</a>) -&gt; {ok, <a href="#type-file_handler">file_handler()</a>} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

Opens a remote file using a handler to let use read and write.
Depending on the mode in use.

<a name="read-2"></a>

### read/2 ###

<pre><code>
read(File_handler::<a href="#type-file_handler">file_handler()</a>, Len::pos_integer()) -&gt; {ok, binary()} | eof | {ok, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

Reads information from an opened remote file.

<a name="read_file-2"></a>

### read_file/2 ###

<pre><code>
read_file(Trooper::<a href="trooper_ssh.md#type-trooper">trooper_ssh:trooper()</a>, Name::string()) -&gt; {ok, binary()} | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

Reads a remote file content.

<a name="rename-3"></a>

### rename/3 ###

<pre><code>
rename(Trooper::<a href="trooper_ssh.md#type-trooper">trooper_ssh:trooper()</a>, OldName::string(), NewName::string()) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

Renames a remote file.

<a name="write-2"></a>

### write/2 ###

<pre><code>
write(File_handler::<a href="#type-file_handler">file_handler()</a>, Data::iolist()) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

Writes information to an opened remote file.

<a name="write_file-3"></a>

### write_file/3 ###

<pre><code>
write_file(Trooper::<a href="trooper_ssh.md#type-trooper">trooper_ssh:trooper()</a>, Name::string(), Content::iolist()) -&gt; ok | {error, <a href="#type-reason">reason()</a>}
</code></pre>
<br />

Writes a remote file content.

