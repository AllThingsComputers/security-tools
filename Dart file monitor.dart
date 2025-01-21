  import 'dart:io';

void main() async {
  var filename = 'FirstFileMon3.txt';
  var myDir = Directory('C:\\Users\\.\\test');
  var timeregex = RegExp(r'2022-11-30 15:38:4[4-8]');

  // List directory contents, recursing into sub-directories, but not following symbolic links.
  await for (var entity in myDir.list(recursive: true, followLinks: false)) {
    if (entity is File) {
      // Get file statistics
      final stat = await entity.stat();
      var accessed = stat.accessed.toString();
      var modified = stat.modified.toString();
      var changed = stat.changed.toString();

      if (timeregex.hasMatch(accessed)) {
        print('File: ${entity.path}');
        print('Accessed: ${stat.accessed}');
        print("This file was recently accessed.");
      } else if (timeregex.hasMatch(modified)) {
        print('File: ${entity.path}');
        print('Modified: ${stat.modified}');
        print("This file was recently modified.");
      } else if (timeregex.hasMatch(changed)) {
        print('File: ${entity.path}');
        print('Changed: ${stat.changed}');
        print("This file was recently changed.");
      }
    }
  }
}
