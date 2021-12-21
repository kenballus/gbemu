#include <chrono>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_set>

#include <SDL2/SDL.h>

#include "GameBoy.hpp"

#define SCREEN_WIDTH 1280
#define SCREEN_HEIGHT 640

typedef Uint32 pixel_t;
pixel_t color_white;
pixel_t color_light_grey;
pixel_t color_dark_grey;
pixel_t color_black;

void error_out(std::string my_error = "") {
    if (my_error != "") {
        std::cout << "My error: " << my_error << std::endl;
    }
    std::cout << "SDL Error: " << SDL_GetError() << std::endl;
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
    for (size_t r = 0; r < GB_SCREEN_HEIGHT; r++) {
        for (size_t c = 0; c < GB_SCREEN_WIDTH; c++) {
            uint8_t color_bits = gb.screen[r * GB_SCREEN_WIDTH + c];
            pixel_t pixel_color;
            switch (color_bits) {
                case 0b00:
                    pixel_color = color_white;
                case 0b01:
                    pixel_color = color_light_grey;
                case 0b10:
                    pixel_color = color_dark_grey;
                case 0b11:
                    pixel_color = color_black;
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
    auto gb = GameBoy();

    std::unordered_set<std::string> args;
    for (int i = 1; i < argc; i++) { args.insert(argv[i]); }

    bool headless = args.contains("--headless");
    args.erase("--headless");

    if (args.size() != 1) {
        std::cerr << "Usage: ./gbemu [--headless] <ROM>\n";
        return 1;
    }
    gb.load_rom(*args.cbegin());

    SDL_Window* window = NULL;
    SDL_Renderer* renderer = NULL;
    SDL_Texture* gb_screen = NULL;
    if (not headless) {
        init(window, renderer, gb_screen);
    }

    while (true) {
        if (not headless) {
            update_gb_screen(gb, gb_screen, renderer, window);

            SDL_Event event;
            while (SDL_PollEvent(&event)) {  // While there are events to process
                if (event.type == SDL_QUIT) {
                    goto done;
                }
            }
        }
        auto ret = gb.execute_instruction(gb.pc);
        if (ret != 0) {
            gb.dump_state();
            gb.dump_mem();
            break;
        }
        gb.wait();
    }
done:

    deinit(window, renderer, gb_screen);
    return 0;
}
