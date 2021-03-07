package odinfmt

import "core:os"
import "core:odin/format"
import "core:fmt"
import "core:strings"
import "flag"

Args :: struct {
    write: Maybe(bool) `flag:"w" usage:"write the new format to file"`,
}

print_help :: proc() {

}

print_arg_error :: proc(error: flag.Flag_Error) {
    fmt.println(error);
}

format_file :: proc(filepath: string) -> ([] u8, bool) {

    if data, ok := os.read_entire_file(filepath); ok {
        return format.format(data, format.default_style);
    }

    else {
        return {}, false;
    }

}

main :: proc() {

    args: Args;

    if len(os.args) < 2 {
        print_help();
        os.exit(1);
    }

    if res := flag.parse(args, os.args[1:len(os.args)-1]); res != .None {
        print_arg_error(res);
        os.exit(1);
    }

    path := os.args[len(os.args)-1];

    if os.is_file(path) {

        if _, ok := args.write.(bool); ok {

            backup_path := strings.concatenate({path, "_bk"}, context.temp_allocator);

            os.rename(path, backup_path);

            if data, ok := format_file(backup_path); ok {
                os.write_entire_file(path, data);
            }

            else {
                fmt.eprintf("failed to write %v", path);
            }

        }

        else {

            if data, ok := format_file(path); ok {
                fmt.println(transmute(string)data);
            }

        }

    }

    else if os.is_dir(path) {

        if _, ok := args.write.(bool); ok {

        }

        else {
            fmt.eprintf("Directory path option requires write flag to be set \n");
            os.exit(1);
        }

    }

    else{
        fmt.eprintf("%v is neither a directory nor a file \n", path);
        os.exit(1);
    }

    os.exit(0);
}