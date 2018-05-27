drop table ratings;
drop table users;
drop table product_images;
drop table products;

--drop type recyclability;
--create type recyclability as enum ('non_recyclable', 'recyclable', 'compostable');

--create table users(
--    id uuid primary key,
--    username text unique not null
--);

create table products(
    id varchar(40) primary key,
--    asin char(10),
    name text not null,
    manufacturer text
);

create table product_images(
    id uuid primary key,
    productid char(13) references products(id),
    image_data bytea not null
);

create table ratings(
    userid uuid not null,
    productid varchar(40) references products(id),
    grade integer not null constraint grade_in_range check(grade >= 1 and grade <= 5),
    vendor text,
    posted timestamp with time zone not null,
    pl_type text not null,
    pl_weight integer not null,
    recyclable text not null
--    recyclable recyclability not null
);
