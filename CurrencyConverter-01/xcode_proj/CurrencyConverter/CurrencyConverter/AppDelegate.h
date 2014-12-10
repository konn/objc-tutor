//
//  AppDelegate.h
//  CurrencyConverter
//
//  Created by 石井 大海 on 2014/12/10.
//  Copyright (c) 2014年 My Name. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface AppDelegate : NSObject <NSApplicationDelegate>

@property (weak) IBOutlet NSTextField *dollarsField;
@property (weak) IBOutlet NSTextField *rateField;
@property (weak) IBOutlet NSTextField *resultField;

- (IBAction)convert:(id)sender;
@end
