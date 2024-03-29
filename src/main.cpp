#include <chrono>
#include <csignal>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_set>

#include <SDL2/SDL.h>

#include "GameBoy.hpp"

#define SCREEN_WIDTH 960
#define SCREEN_HEIGHT 864

#define KEY_MAPPED_TO_A SDLK_a
#define KEY_MAPPED_TO_B SDLK_b
#define KEY_MAPPED_TO_START SDLK_LSHIFT
#define KEY_MAPPED_TO_SELECT SDLK_RSHIFT
#define KEY_MAPPED_TO_UP SDLK_UP
#define KEY_MAPPED_TO_DOWN SDLK_DOWN
#define KEY_MAPPED_TO_LEFT SDLK_LEFT
#define KEY_MAPPED_TO_RIGHT SDLK_RIGHT

typedef Uint32 pixel_t;
pixel_t color_white;
pixel_t color_light_grey;
pixel_t color_dark_grey;
pixel_t color_black;

GameBoy* global_gb;

void error_out(std::string my_error = "") {
    if (my_error != "") {
        std::cerr << "My error: " << my_error << std::endl;
    }
    std::cerr << "SDL Error: " << SDL_GetError() << std::endl;
    exit(1);
}

void sigint_handler(int) {
    global_gb->dump_screen();
    global_gb->dump_mem();
    global_gb->dump_regs();
    exit(1);
}

void init(SDL_Window*& window, SDL_Renderer*& renderer, SDL_Texture*& gb_screen) {
    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        error_out();
    }

    // Make the window. Position not specified. Dimensions specified above. Window shown.
    window = SDL_CreateWindow("GBEMU", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT,
                              SDL_WINDOW_SHOWN);
    if (window == NULL) {
        error_out();
    }

    renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (renderer == NULL) {
        error_out();
    }

    SDL_PixelFormat* format = SDL_AllocFormat(SDL_GetWindowPixelFormat(window));
    if (format == NULL) {
        error_out();
    }

    if (format->BitsPerPixel != sizeof(pixel_t) * 8) {
        error_out("Wrong bpp!");
    }
    color_white = SDL_MapRGB(format, 0xFF, 0xFF, 0xFF);
    color_light_grey = SDL_MapRGB(format, 0xaa, 0xaa, 0xaa);
    color_dark_grey = SDL_MapRGB(format, 0x55, 0x55, 0x55);
    color_black = SDL_MapRGB(format, 0x00, 0x00, 0x00);
    SDL_FreeFormat(format);

    gb_screen = SDL_CreateTexture(renderer, SDL_GetWindowPixelFormat(window), SDL_TEXTUREACCESS_STREAMING,
                                  GB_SCREEN_WIDTH, GB_SCREEN_HEIGHT);
    if (gb_screen == NULL) {
        error_out();
    }

    std::cout << std::hex << std::uppercase;
}

void deinit(SDL_Window*& window, SDL_Renderer*& renderer, SDL_Texture*& gb_screen) {
    SDL_DestroyTexture(gb_screen);
    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);

    SDL_Quit();

    window = NULL;
    renderer = NULL;
    gb_screen = NULL;
}

void update_gb_screen(GameBoy const& gb, SDL_Texture* gb_screen, SDL_Renderer* renderer, SDL_Window* window) {
    void* raw_pixels = NULL;
    int unused = 0;
    SDL_LockTexture(gb_screen, NULL, &raw_pixels, &unused);
    pixel_t* pixels = (pixel_t*)raw_pixels;
    auto [origin_r, origin_c] = gb.get_screen_origin();
    for (size_t r = 0; r < GB_SCREEN_HEIGHT; r++) {
        for (size_t c = 0; c < GB_SCREEN_WIDTH; c++) {
            uint8_t color_bits = gb.screen[(origin_r + r) % (TILE_MAP_WIDTH * TILE_WIDTH)]
                                          [(origin_c + c) % (TILE_MAP_HEIGHT * TILE_HEIGHT)];
            pixel_t pixel_color;
            switch (color_bits) {
                case 0b00:
                    pixel_color = color_white;
                    break;
                case 0b01:
                    pixel_color = color_light_grey;
                    break;
                case 0b10:
                    pixel_color = color_dark_grey;
                    break;
                case 0b11:
                    pixel_color = color_black;
                    break;
            }
            pixels[r * GB_SCREEN_WIDTH + c] = pixel_color;
        }
    }

    SDL_UnlockTexture(gb_screen);
    SDL_RenderCopy(renderer, gb_screen, NULL, NULL);
    SDL_RenderPresent(renderer);
    SDL_UpdateWindowSurface(window);
}

int main(int argc, char* argv[]) {
    std::unordered_set<std::string> args;
    for (int i = 1; i < argc; i++) {
        args.insert(argv[i]);
    }

    bool const headless = args.contains("--headless");
    args.erase("--headless");
    bool debug_paused = args.contains("--freeze");
    args.erase("--freeze");

    if (args.size() != 1) {
        std::cerr << "Usage: " << argv[0] << " [--headless] [--freeze] <ROM>\n";
        return 1;
    }

    auto gb = GameBoy();
    global_gb = &gb;

    gb.load_rom(*args.cbegin());

    SDL_Window* window = NULL;
    SDL_Renderer* renderer = NULL;
    SDL_Texture* gb_screen = NULL;
    if (!headless) {
        init(window, renderer, gb_screen);
    }

    std::signal(SIGINT, sigint_handler);

    uint8_t counter = 0;
    bool stepping = false;
    while (true) {
        if (!headless) {
            if (counter == 0) {
                update_gb_screen(gb, gb_screen, renderer, window);
            }
            SDL_Event event;
            while (SDL_PollEvent(&event)) {  // While there are events to process
                if (event.type == SDL_QUIT) {
                    goto done;
                }

                if (event.type == SDL_KEYDOWN) {
                    switch (event.key.keysym.sym) {
                        case SDLK_RETURN:
                            debug_paused = !debug_paused;
                            break;
                        case SDLK_i:
                            stepping = true;
                            break;
                        case SDLK_s:
                            gb.dump_screen();
                            break;
                        case SDLK_r:
                            gb.dump_regs();
                            break;
                        case SDLK_m:
                            gb.dump_mem();
                            break;
                        case SDLK_v:
                            gb.dump_vram();
                            break;
                        case SDLK_e:
                            std::cout << "IME = " << gb.ime << std::endl;
                            break;
                        case KEY_MAPPED_TO_A:
                            gb.press_button(GB_KEY_A);
                            break;
                        case KEY_MAPPED_TO_B:
                            gb.press_button(GB_KEY_B);
                            break;
                        case KEY_MAPPED_TO_START:
                            gb.press_button(GB_KEY_START);
                            break;
                        case KEY_MAPPED_TO_SELECT:
                            gb.press_button(GB_KEY_SELECT);
                            break;
                        case KEY_MAPPED_TO_UP:
                            gb.press_button(GB_KEY_UP);
                            break;
                        case KEY_MAPPED_TO_DOWN:
                            gb.press_button(GB_KEY_DOWN);
                            break;
                        case KEY_MAPPED_TO_LEFT:
                            gb.press_button(GB_KEY_LEFT);
                            break;
                        case KEY_MAPPED_TO_RIGHT:
                            gb.press_button(GB_KEY_RIGHT);
                            break;
                    }
                } else if (event.type == SDL_KEYUP) {
                    switch (event.key.keysym.sym) {
                        case KEY_MAPPED_TO_A:
                            gb.release_button(GB_KEY_A);
                            break;
                        case KEY_MAPPED_TO_B:
                            gb.release_button(GB_KEY_B);
                            break;
                        case KEY_MAPPED_TO_START:
                            gb.release_button(GB_KEY_START);
                            break;
                        case KEY_MAPPED_TO_SELECT:
                            gb.release_button(GB_KEY_SELECT);
                            break;
                        case KEY_MAPPED_TO_UP:
                            gb.release_button(GB_KEY_UP);
                            break;
                        case KEY_MAPPED_TO_DOWN:
                            gb.release_button(GB_KEY_DOWN);
                            break;
                        case KEY_MAPPED_TO_LEFT:
                            gb.release_button(GB_KEY_LEFT);
                            break;
                        case KEY_MAPPED_TO_RIGHT:
                            gb.release_button(GB_KEY_RIGHT);
                            break;
                    }
                }
            }
        }

        if (stepping || !debug_paused) {
            gb.execute_instruction();
            gb.wait();
            stepping = false;
        }

        counter++;
    }
done:
    // gb.dump_screen();
    // gb.dump_mem();
    // gb.dump_regs();

    deinit(window, renderer, gb_screen);
    error_out("Finished execution.");
    return 0;
}
