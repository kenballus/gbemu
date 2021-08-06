#include <chrono>
#include <iostream>
#include <string>

#include <SDL2/SDL.h>

#include "GameBoy.hpp"

#define SCREEN_WIDTH 1280
#define SCREEN_HEIGHT 640
#define FRAMERATE 60  // in fps

typedef Uint32 pixel_t;
pixel_t color_off;
pixel_t color_on;

void error_out(std::string my_error = "") {
    if (my_error != "") {
        std::cout << "My error: " << my_error << std::endl;
        ;
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
    color_off = SDL_MapRGB(format, 0, 0, 0);
    color_on = SDL_MapRGB(format, 0xFF, 0xFF, 0xFF);
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
    int _ = 0;
    SDL_LockTexture(gb_screen, NULL, &raw_pixels, &_);
    pixel_t* pixels = (pixel_t*)raw_pixels;
    for (size_t r = 0; r < GB_SCREEN_HEIGHT; r++) {
        for (size_t c = 0; c < GB_SCREEN_WIDTH; c++) { pixels[0] = pixels[0]; }
    }

    SDL_UnlockTexture(gb_screen);
    SDL_RenderCopy(renderer, gb_screen, NULL, NULL);
    SDL_RenderPresent(renderer);
    SDL_UpdateWindowSurface(window);
}

void test(GameBoy& gb) {
    // gb.execute_instruction(0b0000111011111111 << 16);  // ld C, FFh
    // gb.execute_instruction(0b0011111011111111 << 16);  // ld A, FFh
    // gb.execute_instruction(0b00000010 << 24);          // ld (BC), A
    gb.set_register(REG_A, 0x3B);
    gb.set_register(REG_H, 0x2A);
    gb.write_mem8(gb.get_register(REG_HL), 0x4F);
    gb.set_flag(FL_C, true);
    gb.sbc_hl_addr();
    gb.dump_mem();
    gb.dump_state();
}

int main(int argc, char* argv[]) {
    SDL_Window* window = NULL;
    SDL_Renderer* renderer = NULL;
    SDL_Texture* gb_screen = NULL;

    init(window, renderer, gb_screen);

    bool quit = false;
    SDL_Event event;
    auto gb = GameBoy();
    test(gb);

    // update_gb_screen(gb, gb_screen, renderer, window);
    // while (!quit) {
    //     while (SDL_PollEvent(&event)) { // While there are events to process
    //         if (event.type == SDL_QUIT) {
    //             quit = true;
    //             break;
    //         }
    //     }
    // }

    deinit(window, renderer, gb_screen);
    return 0;
}
