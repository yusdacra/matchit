use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};

macro_rules! register {
    (colon) => {{
        register!(finish => ":p1", ":p2", ":p3", ":p4")
    }};
    (brackets) => {{
        register!(finish => "{p1}", "{p2}", "{p3}", "{p4}")
    }};
    (regex) => {{
        register!(finish => "(.*)", "(.*)", "(.*)", "(.*)")
    }};
    (finish => $p1:literal, $p2:literal, $p3:literal, $p4:literal) => {{
        let arr = [
            concat!("/authorizations"),
            concat!("/authorizations/", $p1),
            concat!("/applications/", $p1, "/tokens/", $p2),
            concat!("/events"),
            concat!("/repos/", $p1, "/", $p2, "/events"),
            concat!("/networks/", $p1, "/", $p2, "/events"),
            concat!("/orgs/", $p1, "/events"),
            concat!("/users/", $p1, "/received_events"),
            concat!("/users/", $p1, "/received_events/public"),
            concat!("/users/", $p1, "/events"),
            concat!("/users/", $p1, "/events/public"),
            concat!("/users/", $p1, "/events/orgs/", $p2),
            concat!("/feeds"),
            concat!("/notifications"),
            concat!("/repos/", $p1, "/", $p2, "/notifications"),
            concat!("/notifications/threads/", $p1),
            concat!("/notifications/threads/", $p1, "/subscription"),
            concat!("/repos/", $p1, "/", $p2, "/stargazers"),
            concat!("/users/", $p1, "/starred"),
            concat!("/user/starred"),
            concat!("/user/starred/", $p1, "/", $p2),
            concat!("/repos/", $p1, "/", $p2, "/subscribers"),
            concat!("/users/", $p1, "/subscriptions"),
            concat!("/user/subscriptions"),
            concat!("/repos/", $p1, "/", $p2, "/subscription"),
            concat!("/user/subscriptions/", $p1, "/", $p2),
            concat!("/users/", $p1, "/gists"),
            concat!("/gists"),
            concat!("/gists/", $p1),
            concat!("/gists/", $p1, "/star"),
            concat!("/repos/", $p1, "/", $p2, "/git/blobs/", $p3),
            concat!("/repos/", $p1, "/", $p2, "/git/commits/", $p3),
            concat!("/repos/", $p1, "/", $p2, "/git/refs"),
            concat!("/repos/", $p1, "/", $p2, "/git/tags/", $p3),
            concat!("/repos/", $p1, "/", $p2, "/git/trees/", $p3),
            concat!("/issues"),
            concat!("/user/issues"),
            concat!("/orgs/", $p1, "/issues"),
            concat!("/repos/", $p1, "/", $p2, "/issues"),
            concat!("/repos/", $p1, "/", $p2, "/issues/", $p3),
            concat!("/repos/", $p1, "/", $p2, "/assignees"),
            concat!("/repos/", $p1, "/", $p2, "/assignees/", $p3),
            concat!("/repos/", $p1, "/", $p2, "/issues/", $p3, "/comments"),
            concat!("/repos/", $p1, "/", $p2, "/issues/", $p3, "/events"),
            concat!("/repos/", $p1, "/", $p2, "/labels"),
            concat!("/repos/", $p1, "/", $p2, "/labels/", $p3),
            concat!("/repos/", $p1, "/", $p2, "/issues/", $p3, "/labels"),
            concat!("/repos/", $p1, "/", $p2, "/milestones/", $p3, "/labels"),
            concat!("/repos/", $p1, "/", $p2, "/milestones/"),
            concat!("/repos/", $p1, "/", $p2, "/milestones/", $p3),
            concat!("/emojis"),
            concat!("/gitignore/templates"),
            concat!("/gitignore/templates/", $p1),
            concat!("/meta"),
            concat!("/rate_limit"),
            concat!("/users/", $p1, "/orgs"),
            concat!("/user/orgs"),
            concat!("/orgs/", $p1),
            concat!("/orgs/", $p1, "/members"),
            concat!("/orgs/", $p1, "/members", $p2),
            concat!("/orgs/", $p1, "/public_members"),
            concat!("/orgs/", $p1, "/public_members/", $p2),
            concat!("/orgs/", $p1, "/teams"),
            concat!("/teams/", $p1),
            concat!("/teams/", $p1, "/members"),
            concat!("/teams/", $p1, "/members", $p2),
            concat!("/teams/", $p1, "/repos"),
            concat!("/teams/", $p1, "/repos/", $p2, "/", $p3),
            concat!("/user/teams"),
            concat!("/repos/", $p1, "/", $p2, "/pulls"),
            concat!("/repos/", $p1, "/", $p2, "/pulls/", $p3),
            concat!("/repos/", $p1, "/", $p2, "/pulls/", $p3, "/commits"),
            concat!("/repos/", $p1, "/", $p2, "/pulls/", $p3, "/files"),
            concat!("/repos/", $p1, "/", $p2, "/pulls/", $p3, "/merge"),
            concat!("/repos/", $p1, "/", $p2, "/pulls/", $p3, "/comments"),
            concat!("/user/repos"),
            concat!("/users/", $p1, "/repos"),
            concat!("/orgs/", $p1, "/repos"),
            concat!("/repositories"),
            concat!("/repos/", $p1, "/", $p2),
            concat!("/repos/", $p1, "/", $p2, "/contributors"),
            concat!("/repos/", $p1, "/", $p2, "/languages"),
            concat!("/repos/", $p1, "/", $p2, "/teams"),
            concat!("/repos/", $p1, "/", $p2, "/tags"),
            concat!("/repos/", $p1, "/", $p2, "/branches"),
            concat!("/repos/", $p1, "/", $p2, "/branches/", $p3),
            concat!("/repos/", $p1, "/", $p2, "/collaborators"),
            concat!("/repos/", $p1, "/", $p2, "/collaborators/", $p3),
            concat!("/repos/", $p1, "/", $p2, "/comments"),
            concat!("/repos/", $p1, "/", $p2, "/commits/", $p3, "/comments"),
            concat!("/repos/", $p1, "/", $p2, "/commits"),
            concat!("/repos/", $p1, "/", $p2, "/commits/", $p3),
            concat!("/repos/", $p1, "/", $p2, "/readme"),
            concat!("/repos/", $p1, "/", $p2, "/keys"),
            concat!("/repos/", $p1, "/", $p2, "/keys", $p3),
            concat!("/repos/", $p1, "/", $p2, "/downloads"),
            concat!("/repos/", $p1, "/", $p2, "/downloads", $p3),
            concat!("/repos/", $p1, "/", $p2, "/forks"),
            concat!("/repos/", $p1, "/", $p2, "/hooks"),
            concat!("/repos/", $p1, "/", $p2, "/hooks", $p3),
            concat!("/repos/", $p1, "/", $p2, "/releases"),
            concat!("/repos/", $p1, "/", $p2, "/releases/", $p3),
            concat!("/repos/", $p1, "/", $p2, "/releases/", $p3, "/assets"),
            concat!("/repos/", $p1, "/", $p2, "/stats/contributors"),
            concat!("/repos/", $p1, "/", $p2, "/stats/commit_activity"),
            concat!("/repos/", $p1, "/", $p2, "/stats/code_frequency"),
            concat!("/repos/", $p1, "/", $p2, "/stats/participation"),
            concat!("/repos/", $p1, "/", $p2, "/stats/punch_card"),
            concat!("/repos/", $p1, "/", $p2, "/statuses/", $p3),
            concat!("/search/repositories"),
            concat!("/search/code"),
            concat!("/search/issues"),
            concat!("/search/users"),
            concat!("/legacy/issues/search/", $p1, "/", $p2, "/", $p3, "/", $p4),
            concat!("/legacy/repos/search/", $p1),
            concat!("/legacy/user/search/", $p1),
            concat!("/legacy/user/email/", $p1),
            concat!("/users/", $p1),
            concat!("/user"),
            concat!("/users"),
            concat!("/user/emails"),
            concat!("/users/", $p1, "/followers"),
            concat!("/user/followers"),
            concat!("/users/", $p1, "/following"),
            concat!("/user/following"),
            concat!("/user/following/", $p1),
            concat!("/users/", $p1, "/following", $p2),
            concat!("/users/", $p1, "/keys"),
            concat!("/user/keys"),
            concat!("/user/keys/", $p1),
        ];
        std::array::IntoIter::new(arr)
    }};
}

fn call() -> impl Iterator<Item = &'static str> {
    let arr = [
        "/user/repos",
        "/repos/rust-lang/rust/stargazers",
        "/orgs/rust-lang/public_members/nikomatsakis",
        "/repos/rust-lang/rust/releases/1.51.0",
    ];
    std::array::IntoIter::new(arr)
}

fn compare_routers(c: &mut Criterion) {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();

    let mut group = c.benchmark_group("Compare Routers");

    let mut matchit = matchit::Node::new();
    for route in register!(colon) {
        matchit.insert(route, true).unwrap();
    }

    //let gonzales = gonzales::RouterBuilder::new().build(register!(brackets));
    //group.bench_function("gonzales", |b| {
    //    b.iter(|| {
    //        for route in call() {
    //            black_box(gonzales.route(route).unwrap());
    //        }
    //    });
    //});

    //let mut actix = actix_router::Router::<bool>::build();
    //for route in register!(brackets) {
    //    actix.path(route, true);
    //}
    //let actix = actix.finish();
    //group.bench_function("actix", |b| {
    //    b.iter(|| {
    //        for route in call() {
    //            let mut path = actix_router::Path::new(route);
    //            black_box(actix.recognize(&mut path).unwrap());
    //        }
    //    });
    //});

    //let mut route_recognizer = route_recognizer::Router::new();
    //for route in register!(colon) {
    //    route_recognizer.add(route, true);
    //}
    //group.bench_function("route-recognizer", |b| {
    //    b.iter(|| {
    //        for route in call() {
    //            black_box(route_recognizer.recognize(route).unwrap());
    //        }
    //    });
    //});

    async fn handler(_: axum::extract::Body) -> &'static str {
        ""
    }
    use axum::prelude::*;

    let mut axum_router = {
        rt.block_on(async {
            axum::route(concat!("/authorizations"), get(handler))
                .route(concat!("/authorizations/", ":p1"), get(handler))
                .boxed()
                .route(
                    concat!("/applications/", ":p1", "/tokens/", ":p2"),
                    get(handler),
                )
                .route(concat!("/events"), get(handler))
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/events"),
                    get(handler),
                )
                .route(
                    concat!("/networks/", ":p1", "/", ":p2", "/events"),
                    get(handler),
                )
                .route(concat!("/orgs/", ":p1", "/events"), get(handler))
                .route(concat!("/users/", ":p1", "/received_events"), get(handler))
                .route(
                    concat!("/users/", ":p1", "/received_events/public"),
                    get(handler),
                )
                .route(concat!("/users/", ":p1", "/events"), get(handler))
                .route(concat!("/users/", ":p1", "/events/public"), get(handler))
                .boxed()
                .route(
                    concat!("/users/", ":p1", "/events/orgs/", ":p2"),
                    get(handler),
                )
                .route(concat!("/feeds"), get(handler))
                .route(concat!("/notifications"), get(handler))
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/notifications"),
                    get(handler),
                )
                .route(concat!("/notifications/threads/", ":p1"), get(handler))
                .route(
                    concat!("/notifications/threads/", ":p1", "/subscription"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/stargazers"),
                    get(handler),
                )
                .route(concat!("/users/", ":p1", "/starred"), get(handler))
                .route(concat!("/user/starred"), get(handler))
                .route(concat!("/user/starred/", ":p1", "/", ":p2"), get(handler))
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/subscribers"),
                    get(handler),
                )
                .route(concat!("/users/", ":p1", "/subscriptions"), get(handler))
                .route(concat!("/user/subscriptions"), get(handler))
                .boxed()
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/subscription"),
                    get(handler),
                )
                .route(
                    concat!("/user/subscriptions/", ":p1", "/", ":p2"),
                    get(handler),
                )
                .route(concat!("/users/", ":p1", "/gists"), get(handler))
                .route(concat!("/gists"), get(handler))
                .route(concat!("/gists/", ":p1"), get(handler))
                .route(concat!("/gists/", ":p1", "/star"), get(handler))
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/git/blobs/", ":p3"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/git/commits/", ":p3"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/git/refs"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/git/tags/", ":p3"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/git/trees/", ":p3"),
                    get(handler),
                )
                .route(concat!("/issues"), get(handler))
                .route(concat!("/user/issues"), get(handler))
                .route(concat!("/orgs/", ":p1", "/issues"), get(handler))
                .boxed()
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/issues"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/issues/", ":p3"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/assignees"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/assignees/", ":p3"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/issues/", ":p3", "/comments"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/issues/", ":p3", "/events"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/labels"),
                    get(handler),
                )
                .boxed()
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/labels/", ":p3"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/issues/", ":p3", "/labels"),
                    get(handler),
                )
                .boxed()
                .route(
                    concat!(
                        "/repos/",
                        ":p1",
                        "/",
                        ":p2",
                        "/milestones/",
                        ":p3",
                        "/labels"
                    ),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/milestones/"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/milestones/", ":p3"),
                    get(handler),
                )
                .route(concat!("/emojis"), get(handler))
                .route(concat!("/gitignore/templates"), get(handler))
                .route(concat!("/gitignore/templates/", ":p1"), get(handler))
                .route(concat!("/meta"), get(handler))
                .route(concat!("/rate_limit"), get(handler))
                .route(concat!("/users/", ":p1", "/orgs"), get(handler))
                .route(concat!("/user/orgs"), get(handler))
                .route(concat!("/orgs/", ":p1"), get(handler))
                .route(concat!("/orgs/", ":p1", "/members"), get(handler))
                .route(concat!("/orgs/", ":p1", "/members", ":p2"), get(handler))
                .route(concat!("/orgs/", ":p1", "/public_members"), get(handler))
                .route(
                    concat!("/orgs/", ":p1", "/public_members/", ":p2"),
                    get(handler),
                )
                .route(concat!("/orgs/", ":p1", "/teams"), get(handler))
                .route(concat!("/teams/", ":p1"), get(handler))
                .route(concat!("/teams/", ":p1", "/members"), get(handler))
                .route(concat!("/teams/", ":p1", "/members", ":p2"), get(handler))
                .route(concat!("/teams/", ":p1", "/repos"), get(handler))
                .boxed()
                .route(
                    concat!("/teams/", ":p1", "/repos/", ":p2", "/", ":p3"),
                    get(handler),
                )
                .route(concat!("/user/teams"), get(handler))
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/pulls"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/pulls/", ":p3"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/pulls/", ":p3", "/commits"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/pulls/", ":p3", "/files"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/pulls/", ":p3", "/merge"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/pulls/", ":p3", "/comments"),
                    get(handler),
                )
                .route(concat!("/user/repos"), get(handler))
                .route(concat!("/users/", ":p1", "/repos"), get(handler))
                .route(concat!("/orgs/", ":p1", "/repos"), get(handler))
                .route(concat!("/repositories"), get(handler))
                .route(concat!("/repos/", ":p1", "/", ":p2"), get(handler))
                .boxed()
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/contributors"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/languages"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/teams"),
                    get(handler),
                )
                .route(concat!("/repos/", ":p1", "/", ":p2", "/tags"), get(handler))
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/branches"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/branches/", ":p3"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/collaborators"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/collaborators/", ":p3"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/comments"),
                    get(handler),
                )
                .boxed()
                .route(
                    concat!(
                        "/repos/",
                        ":p1",
                        "/",
                        ":p2",
                        "/commits/",
                        ":p3",
                        "/comments"
                    ),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/commits"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/commits/", ":p3"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/readme"),
                    get(handler),
                )
                .route(concat!("/repos/", ":p1", "/", ":p2", "/keys"), get(handler))
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/keys", ":p3"),
                    get(handler),
                )
                .boxed()
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/downloads"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/downloads", ":p3"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/forks"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/hooks"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/hooks", ":p3"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/releases"),
                    get(handler),
                )
                .boxed()
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/releases/", ":p3"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/releases/", ":p3", "/assets"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/stats/contributors"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/stats/commit_activity"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/stats/code_frequency"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/stats/participation"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/stats/punch_card"),
                    get(handler),
                )
                .route(
                    concat!("/repos/", ":p1", "/", ":p2", "/statuses/", ":p3"),
                    get(handler),
                )
                .route(concat!("/search/repositories"), get(handler))
                .route(concat!("/search/code"), get(handler))
                .route(concat!("/search/issues"), get(handler))
                .route(concat!("/search/users"), get(handler))
                .boxed()
                .route(
                    concat!(
                        "/legacy/issues/search/",
                        ":p1",
                        "/",
                        ":p2",
                        "/",
                        ":p3",
                        "/",
                        ":p4"
                    ),
                    get(handler),
                )
                .route(concat!("/legacy/repos/search/", ":p1"), get(handler))
                .route(concat!("/legacy/user/search/", ":p1"), get(handler))
                .route(concat!("/legacy/user/email/", ":p1"), get(handler))
                .route(concat!("/users/", ":p1"), get(handler))
                .route(concat!("/user"), get(handler))
                .boxed()
                .route(concat!("/users"), get(handler))
                .route(concat!("/user/emails"), get(handler))
                .route(concat!("/users/", ":p1", "/followers"), get(handler))
                .route(concat!("/user/followers"), get(handler))
                .route(concat!("/users/", ":p1", "/following"), get(handler))
                .boxed()
                .route(concat!("/user/following"), get(handler))
                .route(concat!("/user/following/", ":p1"), get(handler))
                .route(concat!("/users/", ":p1", "/following", ":p2"), get(handler))
                .route(concat!("/users/", ":p1", "/keys"), get(handler))
                .route(concat!("/user/keys"), get(handler))
                .route(concat!("/user/keys/", ":p1"), get(handler))
        })
    };

    use tower_service::Service;

    let regex_set = regex::RegexSet::new(register!(regex)).unwrap();

    for route in call() {
        group.bench_with_input(BenchmarkId::new("axum", route), route, |b, route| {
            b.to_async(&rt).iter(|| {
                Service::call(
                    &mut axum_router,
                    axum::prelude::Request::builder()
                        .uri(route)
                        .body(axum::body::Body::empty())
                        .unwrap(),
                )
            });
        });

        group.bench_with_input(BenchmarkId::new("matchit", route), route, |b, route| {
            b.to_async(&rt).iter(|| async {
                black_box(matchit.at(route).unwrap());
            });
        });

        group.bench_with_input(BenchmarkId::new("regex", route), route, |b, route| {
            b.to_async(&rt).iter(|| async {
                black_box(regex_set.matches(route));
            });
        });
    }

    group.finish();
}

//fn bench_fibs(c: &mut Criterion) {
//    let mut group = c.benchmark_group("Fibonacci");
//    for i in [20u64, 21u64].iter() {
//        group.bench_with_input(BenchmarkId::new("Recursive", i), i,
//            |b, i| b.iter(|| fibonacci_slow(*i)));
//        group.bench_with_input(BenchmarkId::new("Iterative", i), i,
//            |b, i| b.iter(|| fibonacci_fast(*i)));
//    }
//    group.finish();
//}

criterion_group!(benches, compare_routers);
criterion_main!(benches);
