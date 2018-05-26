drop table ratings;
drop table users;
drop table product_images;
drop table products;
drop type plastic_type;

create type plastic_type as enum('PP', 'PET', 'PVC', 'PTFE');

create table users(
    id uuid primary key,
    username text unique not null
);

create table products(
    id char(13) primary key,
--    asin char(10),
    name text not null,
    manufacturer text
);

create table product_images(
    id serial primary key,
    productid char(13) references products(id),
    image_data bytea not null
);

create table ratings(
    userid uuid references users(id),
    productid char(13) references products(id),
    grade integer constraint grade_in_range check(grade >= 1 and grade <= 5),
    vendor text,
    posted timestamp with time zone,
    pl_type plastic_type,
    pl_weight integer
);
