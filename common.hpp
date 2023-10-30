#pragma once

// Vulkan wrapper
#include <littlevk/littlevk.hpp>

struct pipeline_create_info {
	vk::Device device;
	vk::RenderPass render_pass;
	vk::Extent2D extent;
	vk::PhysicalDeviceMemoryProperties mem_props;
	littlevk::Deallocator *dal;
};

struct pipeline {
	vk::Device device;
	littlevk::Deallocator *dal;
	vk::PhysicalDeviceMemoryProperties mem_props;

	pipeline(const pipeline_create_info &info) : device(info.device), dal(info.dal), mem_props(info.mem_props) {}

	vk::Pipeline ppl;
	vk::PipelineLayout layout;

	virtual void render(const vk::CommandBuffer &) const = 0;
};

