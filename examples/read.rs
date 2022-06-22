use wot_td::thing::Thing;

fn main() {
    let input = std::env::args()
        .nth(1)
        .expect("It needs a path to a thing description file");

    let thing: Thing =
        serde_json::from_reader(std::fs::File::open(input).expect("File not accessible"))
            .expect("Cannot parse the thing");

    println!("{:#?}", thing);
}
