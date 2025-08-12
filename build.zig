const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const llvm = b.option(bool, "llvm", "Use LLVM backend") orelse true;

    const game = b.addModule("gomoku_game", .{
        .root_source_file = b.path("src/game.zig"),
    });
    _ = b.addModule("gomoku_protocol", .{
        .root_source_file = b.path("src/protocol.zig"),
        .imports = &.{
            .{ .name = "gomoku_game", .module = game },
        },
    });

    const test_step_protocol = b.step("test_protocol", "Run protocol unit tests");

    const unit_tests_protocol = b.addTest(.{
        .root_source_file = b.path("src/protocol.zig"),
        .target = target,
        .optimize = optimize,
        .test_runner = .{ .path = b.path("test_runner.zig"), .mode = .simple },
        .use_llvm = llvm,
    });

    unit_tests_protocol.root_module.addImport("gomoku_game", game);

    const run_unit_tests_protocol = b.addRunArtifact(unit_tests_protocol);
    run_unit_tests_protocol.has_side_effects = true;
    test_step_protocol.dependOn(&run_unit_tests_protocol.step);

    const test_step_game = b.step("test_game", "Run game unit tests");

    const unit_tests_game = b.addTest(.{
        .root_source_file = b.path("src/game.zig"),
        .target = target,
        .optimize = optimize,
        .test_runner = .{ .path = b.path("test_runner.zig"), .mode = .simple },
        .use_llvm = llvm,
    });

    const run_unit_tests_game = b.addRunArtifact(unit_tests_game);
    run_unit_tests_game.has_side_effects = true;
    test_step_game.dependOn(&run_unit_tests_game.step);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests_game.step);
    test_step.dependOn(&run_unit_tests_protocol.step);
}
