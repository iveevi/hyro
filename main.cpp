#include <iostream>

#include <glm/glm.hpp>

// Parsing
#include <lexy/action/parse.hpp>
#include <lexy/action/trace.hpp>
#include <lexy/callback.hpp>
#include <lexy/callback/string.hpp>
#include <lexy/dsl.hpp>
#include <lexy/dsl/integer.hpp>
#include <lexy/dsl/literal.hpp>
#include <lexy/input/file.hpp>
#include <lexy/input/string_input.hpp>
#include <lexy_ext/report_error.hpp>

// Printing
#include <fmt/format.h>

// Local headers
#include "common.hpp"

struct element {
	using copy_callback = std::function <element *(const element *)>;
	using pipeline_constructor = std::function <pipeline *(const pipeline_create_info &)>;
	using pipeline_insert = std::function <void (pipeline *, const element *)>;

	const char           header[4] = "HEL";
	copy_callback        copy;
	pipeline_constructor ppl_ctor;
	pipeline_insert      ppl_insert;
	const char           *ppl_name;

	// TODO: callbacks
	// pipeline construction callback

	element(const char *ppl_name, const copy_callback &copy, const pipeline_constructor &ppl_ctor, const pipeline_insert &ppl_insert)
			: copy(copy), ppl_ctor(ppl_ctor), ppl_name(ppl_name), ppl_insert(ppl_insert) {}
};

struct circle : element {
	glm::vec2 min;
	glm::vec2 max;

	circle(const glm::vec2 &min, const glm::vec2 &max)
			: element("__circle", copy, ppl_ctor, ppl_insert), min(min), max(max) {}

	static circle   *copy(const element *);
	static pipeline *ppl_ctor(const pipeline_create_info &);
	static void     ppl_insert(pipeline *, const element *);

	// TODO: constructor, and pybind11 like binding interface
	// also encode custom type info?
};

// TODO: also need a window context for each pipeline -> window
struct window_creation_info {
	std::string title;
	vk::Extent2D extent;
	std::vector <element *> elements;
};

struct circle_pipeline : pipeline {
	// Push constants
	struct push_constants {
		glm::vec3 color;
	};

	// Vertex properties
	static constexpr vk::VertexInputBindingDescription vertex_binding {
		0, 2 * sizeof(glm::vec2), vk::VertexInputRate::eVertex,
	};

	static constexpr std::array <vk::VertexInputAttributeDescription, 2> vertex_attributes {
		vk::VertexInputAttributeDescription {
			0, 0, vk::Format::eR32G32Sfloat, 0
		},
		vk::VertexInputAttributeDescription {
			1, 0, vk::Format::eR32G32Sfloat, sizeof(glm::vec2)
		},
	};

	// Inlined shaders
	// static cppsl::result vertex_shader() {
	// 	using namespace cppsl;
	//
	// 	layout_input <vec2, 0> position;
	// 	layout_input <vec2, 1> uv;
	//
	// 	return { gl_Position(position, 0, 1), layout_output <vec2, 0> (uv.x, uv.y) };
	// 	// return { gl_Position(position, 0, 1), layout_output <vec2, 0> (uv) };
	// }

	// static cppsl::result fragment_shader() {
	// 	using namespace cppsl;
	//
	// 	layout_input <vec2, 0> uv;
	// 	f32 intensity = 1.0f - f32(uv.x * uv.x + uv.y * uv.y); // TODO: lenght function
	// 	return { layout_output <vec4, 0> (intensity) };
	// }

	static constexpr char vertex_shader[] = R"(
#version 450

layout (location = 0) in vec2 position;
layout (location = 1) in vec2 uv;

layout (location = 0) out vec2 frag_uv;

void main()
{
	gl_Position = vec4(position, 0.0f, 1.0f);
	frag_uv = uv;
}
	)";

	static constexpr char fragment_shader[] = R"(
#version 450

layout (location = 0) in vec2 uv;

layout (location = 0) out vec4 fragment;

void main()
{
	float intensity = 4.0f * (uv.x * uv.x + uv.y * uv.y);
	intensity = intensity < 1.0f ? 1.0f : 0.0f;
	fragment = vec4(intensity);
}
	)";

	circle_pipeline(const pipeline_create_info &info) : pipeline(info) {
		// Compile shader modules
		// auto svertex = cppsl::transcribe(vertex_shader());
		// auto sfragment = cppsl::transcribe(fragment_shader());

		vk::ShaderModule vertex_module = littlevk::shader::compile(
			info.device, std::string(vertex_shader),
			vk::ShaderStageFlagBits::eVertex
		).unwrap(info.dal);

		vk::ShaderModule fragment_module = littlevk::shader::compile(
			info.device, std::string(fragment_shader),
			vk::ShaderStageFlagBits::eFragment
		).unwrap(info.dal);

		// Create the pipeline
		vk::PushConstantRange push_constant_range {
			vk::ShaderStageFlagBits::eVertex,
			0, sizeof(push_constants)
		};

		layout = littlevk::pipeline_layout(
			info.device,
			vk::PipelineLayoutCreateInfo {
				{}, {}, push_constant_range
			}
		).unwrap(info.dal);

		littlevk::pipeline::GraphicsCreateInfo pipeline_info;
		pipeline_info.vertex_binding = vertex_binding;
		pipeline_info.vertex_attributes = vertex_attributes;
		pipeline_info.vertex_shader = vertex_module;
		pipeline_info.fragment_shader = fragment_module;
		pipeline_info.extent = info.extent;
		pipeline_info.pipeline_layout = layout;
		pipeline_info.render_pass = info.render_pass;
		// pipeline_info.fill_mode = vk::PolygonMode::eLine;
		pipeline_info.cull_mode = vk::CullModeFlagBits::eNone;

		ppl = littlevk::pipeline::compile(info.device, pipeline_info).unwrap(info.dal);
	}

	std::vector <circle> circles;
	littlevk::Buffer dev_circles;

	void push(const circle &);
	void render(const vk::CommandBuffer &) const override;
};

circle *circle::copy(const element *e)
{
	return new circle(*static_cast <const circle *> (e));
}

pipeline *circle::ppl_ctor(const pipeline_create_info &info)
{
	return new circle_pipeline(info);
}

void circle::ppl_insert(pipeline *ppl, const element *e)
{
	static_cast <circle_pipeline *> (ppl)->push(*static_cast <const circle *> (e));
}

void circle_pipeline::push(const circle &circle)
{
	circles.push_back(circle);

	std::vector <glm::vec2> vertices;
	for (const auto &circle : circles) {
		// Triangle one
		vertices.push_back({ circle.min.x, circle.min.y });
		vertices.push_back({ -0.5, -0.5 });

		vertices.push_back({ circle.min.x, circle.max.y });
		vertices.push_back({ -0.5, 0.5 });

		vertices.push_back({ circle.max.x, circle.min.y });
		vertices.push_back({ 0.5, -0.5 });

		// Triangle two
		vertices.push_back({ circle.max.x, circle.min.y });
		vertices.push_back({ 0.5, -0.5 });

		vertices.push_back({ circle.max.x, circle.max.y });
		vertices.push_back({ 0.5, 0.5 });

		vertices.push_back({ circle.min.x, circle.max.y });
		vertices.push_back({ -0.5, 0.5 });
	}

	dev_circles = littlevk::buffer(device,
		vertices,
		vk::BufferUsageFlagBits::eVertexBuffer,
		mem_props
	).unwrap(dal);
}

void circle_pipeline::render(const vk::CommandBuffer &cmd) const
{
	cmd.bindPipeline(vk::PipelineBindPoint::eGraphics, ppl);
	cmd.bindVertexBuffers(0, *dev_circles, { 0 });
	cmd.draw(circles.size() * 6, 1, 0, 0);
}

// polygon?
struct Window : littlevk::Skeleton {
	vk::PhysicalDevice phdev;
	vk::PhysicalDeviceMemoryProperties mem_props;

	littlevk::Deallocator *dal = nullptr;

	vk::RenderPass render_pass;
	vk::CommandPool command_pool;

	std::vector <vk::Framebuffer> framebuffers;
	std::vector <vk::CommandBuffer> command_buffers;

	littlevk::PresentSyncronization sync;

	// Pipelines
	pipeline_create_info ppl_cinfo;

	std::unordered_map <std::string, pipeline *> pipelines;
	// circle_pipeline *c_ppl;

	Window(const vk::PhysicalDevice &phdev_, const window_creation_info &wci) : phdev(phdev_) {
		mem_props = phdev.getMemoryProperties();
		// skeletonize(phdev, { 1000, 1000 }, "Final");
		skeletonize(phdev, wci.extent, wci.title);

		dal = new littlevk::Deallocator(device);

		// Create the render pass
		std::array <vk::AttachmentDescription, 1> attachments {
			littlevk::default_color_attachment(swapchain.format),
		};

		std::array <vk::AttachmentReference, 1> color_attachments {
			vk::AttachmentReference {
				0, vk::ImageLayout::eColorAttachmentOptimal,
			}
		};

		vk::SubpassDescription subpass {
			{}, vk::PipelineBindPoint::eGraphics,
			{}, color_attachments,
			{}, nullptr
		};

		render_pass = littlevk::render_pass(
			device,
			vk::RenderPassCreateInfo {
				{}, attachments, subpass
			}
		).unwrap(dal);

		// Create framebuffers from the swapchain
		littlevk::FramebufferSetInfo fb_info;
		fb_info.swapchain = &swapchain;
		fb_info.render_pass = render_pass;
		fb_info.extent = window->extent;

		framebuffers = littlevk::framebuffers(device, fb_info).unwrap(dal);

		// Allocate command buffers
		command_pool = littlevk::command_pool(device,
			vk::CommandPoolCreateInfo {
				vk::CommandPoolCreateFlagBits::eResetCommandBuffer,
				littlevk::find_graphics_queue_family(phdev)
			}
		).unwrap(dal);

		command_buffers = device.allocateCommandBuffers({
			command_pool, vk::CommandBufferLevel::ePrimary, 2
		});

		// Present syncronization
		sync = littlevk::present_syncronization(device, 2).unwrap(dal);

		// Compile pipelines
		ppl_cinfo = pipeline_create_info {
			device, render_pass,
			window->extent,
			mem_props, dal
		};

		// c_ppl = new circle_pipeline(info);
		// c_ppl->push(circle { { 0.1, 0.1 }, { 0.5, 0.5 } });
	}

	~Window() {
		printf("Destroying window: %s\n", window->title.c_str());
		device.waitIdle();
		// delete c_ppl;
		delete dal;
	}

	// No copy operations
	Window(const Window &) = delete;
	Window &operator=(const Window &) = delete;

	// Move only
	Window(Window &&) = default;
	Window &operator=(Window &&) = default;

	// Adding elements via pipelines
	void push(const element *e) {
		// Compile the pipeline if needed
		if (!pipelines.count(e->ppl_name)) {
			// TODO: ulog
			printf("Compiling pipeline: %s\n", e->ppl_name);
			pipelines[e->ppl_name] = e->ppl_ctor(ppl_cinfo);
		}

		// Add the element
		e->ppl_insert(pipelines[e->ppl_name], e);
	}

	size_t frame = 0;

	void render() {
		// Get next image
		littlevk::SurfaceOperation op;
                op = littlevk::acquire_image(device, swapchain.swapchain, sync, frame);

		// Record command buffer
		vk::CommandBuffer &cmd = command_buffers[frame];

		vk::RenderPassBeginInfo render_pass_info = littlevk::default_rp_begin_info <1>
				(render_pass, framebuffers[op.index], window);

		cmd.begin(vk::CommandBufferBeginInfo {});
		cmd.beginRenderPass(render_pass_info, vk::SubpassContents::eInline);

		// c_ppl->render(cmd);
		for (auto &p : pipelines)
			p.second->render(cmd);

		cmd.endRenderPass();
		cmd.end();

		// Submit command buffer while signaling the semaphore
		vk::PipelineStageFlags wait_stage = vk::PipelineStageFlagBits::eColorAttachmentOutput;

		vk::SubmitInfo submit_info {
			1, &sync.image_available[frame],
			&wait_stage,
			1, &cmd,
			1, &sync.render_finished[frame]
		};

		graphics_queue.submit(submit_info, sync.in_flight[frame]);

		// Send image to the screen
		op = littlevk::present_image(present_queue, swapchain.swapchain, sync, op.index);
		frame = 1 - frame;
	}

	bool should_close() {
		return glfwWindowShouldClose(window->handle);
	}

	void poll() {
		glfwPollEvents();
	}
};

// Type system for hyro
struct expression_value;
struct constructor_info;

using expression_list = std::vector <expression_value>;

struct structure_instance {
	// This is the header information
	static constexpr size_t HEADER_SIZE = 4;
	static constexpr size_t DATA_OFFSET = (HEADER_SIZE + 2 * sizeof(int64_t) - 1) / sizeof(int64_t) * sizeof(int64_t);

	const char header[HEADER_SIZE] = "HST";
	int64_t    resolved_type = -1;

	std::byte *deferred() {
		return reinterpret_cast <std::byte *> (this) + DATA_OFFSET;
	}
};

template <typename T>
struct wrapped_instance : structure_instance {
	T data;

	wrapped_instance(const T &data_) : data(data_) {}

	template <typename... Args>
	wrapped_instance(Args &&... args) : data(std::forward <Args> (args)...) {}
};

struct structure {
	int64_t resolved_type;
	int64_t size;

	struct member_info {
		size_t resolved_type;
		size_t offset;
	};

	std::unordered_map <std::string, member_info> members;
};

struct identifier_info : std::vector <std::string> {
	using std::vector <std::string> ::vector;

	std::string str() const {
		std::string str;
		for (size_t i = 0; i < size(); i++)
			str += (i ? "." : "") + (*this)[i];
		return str;
	}
};

struct type_info {
	static expression_value construct(const constructor_info &);
};

struct function_info {
	using arguments = std::vector <expression_value>;

	static expression_value call(const identifier_info &, const arguments &);
};

#define EXPRESSION_AGGREGATES std::variant <double, glm::vec2, std::string, structure_instance *, expression_list>

struct expression_value : EXPRESSION_AGGREGATES {
	using EXPRESSION_AGGREGATES ::variant;

	expression_value(const constructor_info &info) {
		*this = type_info::construct(info);
	}

	std::string str() const {
		if (auto d = std::get_if <double> (this)) {
			return std::to_string(*d);
		} else if (auto v = std::get_if <glm::vec2> (this)) {
			return "vec2(" + std::to_string(v->x) + ", " + std::to_string(v->y) + ")";
		} else if (auto s = std::get_if <std::string> (this)) {
			return "\"" + *s + "\"";
		} else if (auto s = std::get_if <structure_instance *> (this)) {
			// TODO: safer check
			std::string ok = std::strcmp((*s)->header, "HST") == 0 ? "OK" : "BAD";
			std::string type = (*s)->resolved_type == -1 ? "nil" : std::to_string((*s)->resolved_type);
			return fmt::format("struct <{}, {}> (@{})", ok, type, (void *) s);
		} else if (auto l = std::get_if <expression_list> (this)) {
			std::string ret = "[";
			for (size_t i = 0; i < l->size(); i++)
				ret += (i ? ", " : "") + (*l)[i].str();
			ret += "]";
			return ret;
		} else {
			return "<?>";
		}
	}

	template <typename T>
	T &get() {
		// TODO: option type using get_if
		return std::get <T> (*this);
	}

	template <typename T>
	const T &get() const {
		return std::get <T> (*this);
	}
};

struct member_initializer_info {
	std::string member;
	expression_value value;
};

struct constructor_info {
	// std::string type;
	identifier_info type;
	std::unordered_map <std::string, expression_value> args;

	constructor_info(identifier_info &&type, std::vector <member_initializer_info> &&args_)
			: type(std::move(type)) {
		for (auto &m : args_)
			args.emplace(std::move(m.member), std::move(m.value));
	}

	std::string str() const {
		std::string ret = type.str() + " {\n";
		for (auto &[member, value] : args)
			ret += "  " + member + " = " + value.str() + "\n";
		ret += "}";
		return ret;
	}
};

// Built-in types
expression_value type_info::construct(const constructor_info &info)
{
	using constructor_args = decltype(info.args);
	using constructor = std::function <expression_value (constructor_args)>;

	static std::unordered_map <std::string, constructor> map {
		// TODO: makr used args as used, and warn about unused args
		{
			"float",
			[](const constructor_args &a) {
				return std::get <double> (a.at("self"));
			}
		},

		{
			"circle",
			[](const constructor_args &a) {
				glm::vec2 min = std::get <glm::vec2> (a.at("min"));
				glm::vec2 max = std::get <glm::vec2> (a.at("max"));
				printf("constructing a circle:\n");
				printf("  > min = %f, %f\n", min.x, min.y);
				printf("  > max = %f, %f\n", max.x, max.y);
				return new wrapped_instance <circle> (min, max);
			}
		},

		{
			"window",
			[](const constructor_args &a) {
				glm::vec2 size = std::get <glm::vec2> (a.at("size"));
				std::string title = std::get <std::string> (a.at("title"));
				printf("constructing a window:\n");
				printf("  > size = %f, %f\n", size.x, size.y);
				printf("  > title = \"%s\"\n", title.c_str());
				printf("  > elements: %s\n", a.at("elements").str().c_str());

				// TODO: assertion for expected (in initializer list here) types...

				// Ensure that each element is a window element
				std::vector <element *> elements;
				for (auto &e : a.at("elements").get <expression_list> ()) {
					static char HEL[] = "HEL";
					printf("  > element: %s\n", e.str().c_str());

					wrapped_instance <element> *we = (wrapped_instance <element> *) e.get <structure_instance *> ();
					// TODO: make sure it is a structure...
					const char *header = we->data.header;

					static char buffer[4];
					// TODO: expand macro...
					buffer[0] = header[0];
					buffer[1] = header[1];
					buffer[2] = header[2];
					buffer[3] = '\0';

					if (std::strcmp(buffer, HEL) != 0) {
						fprintf(stderr, "Expected a window element, got %s\n", buffer);
						exit(1);
					} else {
						const element::copy_callback &copy = we->data.copy;
						printf("  > element is a window element\n");
						element *e = copy(&we->data);
						elements.push_back(e);
					}
				}

				vk::Extent2D extent = { (unsigned int) size.x, (unsigned int) size.y };
				return new wrapped_instance <window_creation_info> (title, extent, elements);
			}
		}
	};

	std::string resolved = info.type.str();
	if (map.find(resolved) != map.end())
		return map[resolved](info.args);

	// TODO: micro log
	fmt::print("Unknown type: {}\n", resolved);
	exit(1);
}

// Evaluation state
struct hyro_evaluation_state {
	vk::PhysicalDevice physical_device;

	// TODO: need to account for nested scopes
	std::unordered_map <std::string, expression_value> variables;

	// Final results are windows with elements
	std::vector <window_creation_info> windows;

	void dump() const {
		printf("State:\n");
		for (auto &v : variables)
			printf("  > %s = %s\n", v.first.c_str(), v.second.str().c_str());
	}
} hyro_state;

// Bult-in functions
expression_value function_info::call(const identifier_info &name, const arguments &args)
{
	using function_type = std::function <expression_value (const arguments &)>;

	static std::unordered_map <std::string, function_type> map {
		{
			"println",
			[](const arguments &args) {
				for (auto &arg : args)
					fmt::print("{} ", arg.str());
				fmt::print("\n");
				return nullptr;
			}
		},

		// TODO: how to deal with namespaces/scopes?
		// TODO: need a state
		{
			"final.push",
			[](const arguments &args) {
				printf("final.push with:\n");
				for (auto &arg : args) {
					printf("  > %s\n", arg.str().c_str());

					// TODO: verify the type...
					structure_instance *si = std::get <structure_instance *> (arg);
					wrapped_instance <window_creation_info> *wi = (wrapped_instance <window_creation_info> *) si;
					const window_creation_info &wci = wi->data;

					hyro_state.windows.push_back(wci);
				}

				return nullptr;
			}
		}
	};

	std::string resolved = name.str();
	if (map.find(resolved) != map.end())
		return map[resolved](args);

	fmt::print("Unknown function: {}\n", resolved);
	exit(1);
}

struct assignment_info {
	identifier_info name;
	expression_value value; // TODO: for native types, use "self: <expression_value>"

	assignment_info(identifier_info &&name, expression_value &&value)
			: name(std::move(name)), value(std::move(value)) {
		fmt::print("assignment_info: {}\n", str());
	}

	std::string str() const {
		return name.str() + " = " + value.str();
	}
};

struct invocation_info {
	identifier_info function;
	std::vector <expression_value> arguments;

	invocation_info(identifier_info &&function, std::vector <expression_value> &&arguments)
			: function(std::move(function)), arguments(std::move(arguments)) {
		fmt::print("invocation_info: {}\n", str());
	}

	std::string str() const {
		std::string ret = function.str() + "(";
		for (size_t i = 0; i < arguments.size(); ++i) {
			ret += arguments[i].str();
			if (i < arguments.size() - 1)
				ret += ", ";
		}
		ret += ")";
		return ret;
	}
};

enum statement_type {
	eError,
	eAssignment,
	eInvocation
};

struct statement_info : std::variant <assignment_info, invocation_info> {
	using std::variant <assignment_info, invocation_info> ::variant;

	std::string str() const {
		if (auto assignment = std::get_if <assignment_info> (this))
			return assignment->str();
		else if (auto invocation = std::get_if <invocation_info> (this))
			return invocation->str();
		else
			return "?";
	}

	template <typename T>
	const T &get() const {
		return std::get <T> (*this);
	}

	statement_type type() const {
		if (std::holds_alternative <assignment_info> (*this))
			return eAssignment;
		else if (std::holds_alternative <invocation_info> (*this))
			return eInvocation;
		return eError;
	}
};

namespace {

namespace grammar {

namespace dsl = lexy::dsl;

struct real {
	static constexpr auto rule = [](void) {
		auto integer_part = dsl::sign + dsl::digits <>;
		auto fraction = dsl::period >> dsl::digits<>;
		auto exponent = (dsl::lit_c <'e'> / dsl::lit_c <'E'>) >> dsl::sign + dsl::digits <>;
		auto real_number = dsl::token(integer_part + dsl::if_(fraction) + dsl::if_(exponent));
		return dsl::capture(real_number);
	}();

	static constexpr auto value = lexy::as_string <std::string>
		| lexy::callback <double> (
			[](std::string &&str) {
				return std::stod(str);
			}
		);
};

// NOTE: this can be generalized to N dimensions
struct vec2 {
	static constexpr auto whitespace = dsl::ascii::space;

	static constexpr auto rule = [](void) {
		auto header = dsl::lit <"vec2">;
		auto separator = dsl::sep(dsl::comma);
		return header + dsl::lit_c <'('> + dsl::times <2> (dsl::p <real>, separator) + dsl::lit_c <')'>;
	} ();

	static constexpr auto value = lexy::construct <glm::vec2>;
};

struct string {
	static constexpr auto char_type = dsl::ascii::print;
	static constexpr auto rule = lexy::dsl::quoted(char_type);
	static constexpr auto value = lexy::as_string <std::string>;
};

struct simple_identifier {
	static constexpr auto rule = [](void) {
		auto head = dsl::ascii::alpha_underscore;
		auto tail = dsl::ascii::alpha_digit_underscore;
		return dsl::identifier(head, tail);
	} ();

	static constexpr auto value = lexy::as_string <std::string>;
};

struct identifier {
	static constexpr auto rule = [](void) {
		auto separator = dsl::sep(dsl::period);
		return dsl::list(dsl::p <simple_identifier>, separator);
	} ();

	static constexpr auto value = lexy::fold_inplace <identifier_info> (
		identifier_info(),
		[](identifier_info &info, std::string &&str) {
			info.push_back(std::move(str));
		}
	);
};

struct identifier_reference {
	static constexpr auto rule = dsl::p <identifier>;
	static constexpr auto value = lexy::forward <identifier_info>
		| lexy::callback <expression_value> (
			[](identifier_info &&info) {
				auto v = hyro_state.variables[info.str()];
				printf("identifier_reference: %s -> %s\n", info.str().c_str(), v.str().c_str());
				return v;
			}
		);
};

struct list;
struct constructor;

struct expression {
	static constexpr auto rule = [](void) {
		auto ws = dsl::whitespace(dsl::ascii::space);

		auto real_ = dsl::p <real>;
		auto vec2_ = dsl::peek(dsl::lit <"vec2">) >> dsl::p <vec2>;
		auto string_ = dsl::peek(dsl::lit_c <'"'>) >> dsl::p <string>;
		auto list_ = dsl::peek(dsl::lit_c <'['>) >> dsl::recurse <list>;
		auto struct_ = dsl::peek(dsl::p <identifier> + ws + dsl::lit_c <'{'>) >> dsl::recurse <constructor>;
		auto ref_ = dsl::peek(dsl::p <identifier>) >> dsl::p <identifier_reference>;

		return real_ | vec2_ | string_ | list_ | struct_ | ref_;
	} ();

	static constexpr auto value = lexy::construct <expression_value>;
};

struct list {
	constexpr static auto whitespace = dsl::ascii::space;

	static constexpr auto rule = [](void) {
		auto separator = dsl::sep(dsl::comma);
		return dsl::lit_c <'['>
			+ dsl::list(dsl::p <expression>, separator)
			+ dsl::lit_c <']'>;
	} ();

	static constexpr auto value = lexy::as_list <std::vector <expression_value>>;
};

struct member_initializer {
	static constexpr auto whitespace = dsl::ascii::space;

	static constexpr auto rule = [](void) {
		return dsl::p <simple_identifier> + dsl::lit_c <':'> + dsl::p <expression>;
	} ();

	static constexpr auto value = lexy::construct <member_initializer_info>;
};

struct member_initializer_list {
	static constexpr auto whitespace = dsl::ascii::space;

	static constexpr auto rule = [](void) {
		auto separator = dsl::sep(dsl::comma);
		return dsl::lit_c <'{'>
			+ dsl::list(dsl::p <member_initializer>, separator)
			+ dsl::lit_c <'}'>;
	} ();

	static constexpr auto value = lexy::as_list <std::vector <member_initializer_info>>;
};

struct constructor {
	static constexpr auto whitespace = dsl::ascii::space;

	static constexpr auto rule = [](void) {
		return dsl::p <identifier> + dsl::p <member_initializer_list>;
	} ();

	static constexpr auto value = lexy::construct <constructor_info>;
};

struct assignment {
	static constexpr auto whitespace = dsl::ascii::space;

	static constexpr auto rule = [](void) {
		return dsl::p <identifier> + dsl::lit_c <'='> + dsl::p <expression>;
	} ();

	static constexpr auto value = lexy::construct <assignment_info>
		| lexy::forward <assignment_info>
		| lexy::callback <assignment_info> (
			[](assignment_info &&info) {
				printf("assignment: %s = %s\n", info.name.str().c_str(), info.value.str().c_str());
				hyro_state.variables[info.name.str()] = info.value;
				return info;
			}
		);
};

struct argument_list {
	static constexpr auto whitespace = dsl::ascii::space;

	static constexpr auto rule = [](void) {
		auto separator = dsl::sep(dsl::comma);
		return dsl::lit_c <'('> + dsl::list(dsl::p <expression>, separator) + dsl::lit_c <')'>;
	} ();

	static constexpr auto value = lexy::as_list <std::vector <expression_value>>;
};

// TODO: this should be a kind of expression...
// then all statements are assignments or expresssions...
struct invocation {
	static constexpr auto whitespace = dsl::ascii::space;

	static constexpr auto rule = [](void) {
		return dsl::p <identifier> + dsl::p <argument_list>;
	} ();

	static constexpr auto value = lexy::construct <invocation_info>
		| lexy::forward <invocation_info>
		| lexy::callback <invocation_info> (
			[](invocation_info &&info) {
				printf("invocation: %s (%lu)\n", info.function.str().c_str(), info.arguments.size());
				function_info::call(info.function, info.arguments);
				return info;
			}
		);
};

struct statement {
	static constexpr auto whitespace = dsl::ascii::space;

	static constexpr auto rule = [](void) {
		auto ws = dsl::whitespace(dsl::ascii::space);
		auto teaser = dsl::p <identifier> + ws + dsl::lit_c <'='>;
		return dsl::peek(teaser) >> dsl::p <assignment>
			| dsl::else_ >> dsl::p <invocation>;
	} ();

	// static constexpr auto value = assignment::value;
	static constexpr auto value = lexy::construct <statement_info>;
};

struct grammar {
	static constexpr auto whitespace = dsl::ascii::space;

	static constexpr auto rule = [](void) {
		return dsl::list(dsl::peek_not(dsl::eof) >> dsl::p <statement>) + dsl::eof;
	} ();

	static constexpr auto value = lexy::as_list <std::vector <statement_info>>;
};

using production = grammar;

// TODO: get trailing comma to work...

}

}

int main()
{
	// Pick a physical device
	auto predicate = [](vk::PhysicalDevice phdev) {
		return littlevk::physical_device_able(phdev, {
			VK_KHR_SWAPCHAIN_EXTENSION_NAME
		});
	};

	hyro_state.physical_device = littlevk::pick_physical_device(predicate);
	// vk::PhysicalDevice phdev = littlevk::pick_physical_device(predicate);

	// Parse the script
	auto file = lexy::read_file <lexy::utf8_encoding> ("../simple.hyro");
	if (!file) {
		// TODO: microlog...
		fprintf(stderr, "Failed to open file\n");
		return 1;
	}

	auto input = file.buffer();
	auto result = lexy::parse <grammar::production> (input, lexy_ext::report_error);

	// if (result) {
	// 	printf("Success!\n");
	// 	for (auto &s : result.value()) {
	// 		printf("  > %s (%s)\n", s.str().c_str(), s.type() == eAssignment ? "assignment" : "invocation");
	//
	// 		if (s.type() == eAssignment) {
	// 			const assignment_info &ai = s.get <assignment_info> ();
	// 			hyro_state.variables[ai.name.str()] = ai.value;
	// 			hyro_state.dump();
	// 		} else {
	// 			const invocation_info &ii = s.get <invocation_info> ();
	// 			printf("    > %s (%zu)\n", ii.function.str().c_str(), ii.arguments.size());
	// 			function_info::call(ii.function, ii.arguments);
	// 		}
	// 	}
	// } else {
	// 	printf("Failure!\n");
	// }

	hyro_state.dump();

	// lexy::trace <grammar::production> (stdout, input);

	// Instantiate all windows
	if (hyro_state.windows.empty()) {
		fprintf(stderr, "No windows to render\n");
		return 1;
	}

	std::vector <Window> windows;
	windows.reserve(hyro_state.windows.size());

	for (const auto &wci : hyro_state.windows) {
		printf("Pushing window: %s\n", wci.title.c_str());
		windows.emplace_back(hyro_state.physical_device, wci);

		// Add elements to this window
		for (const auto &e : wci.elements) {
			printf("  > Pushing element: %p\n", e);
			windows.back().push(e);
		}
	}

	bool should_close;
	do {
		should_close = false;
		for (auto &w : windows) {
			w.render();
			w.poll();
			should_close |= w.should_close();
		}
	} while (!should_close);
}
